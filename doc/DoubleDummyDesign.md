# Bridge Double-Dummy Solver — Design Document

*Reflects the state of the project as of version 1.1.4.*

## Overview

This document describes the design of the double-dummy solver for the Bridge
project, which uses the Gambit framework (`AlphaBetaPlayer`) to perform
exhaustive minimax search over all possible card plays, with both sides assumed
to play perfectly with full knowledge of all four hands.

The solver is integrated via the Whist model — a simplified bridge game without
auction — and is accessed through `Whist.analyzeDoubleDummy`.

**Update, 2026-07-14**: everything below this point up to "The Bitboard Engine"
describes the original object-graph engine (`State`/`WhistGame`/`WhistState`),
still in place and still the trusted reference for cross-validation. A second,
bitboard-based engine (`BitState`/`BitWhistGame`/`BitAnalysis`, package
`bridge.cards.bits` / `bridge.gambit.bits`) was added afterwards, running
*alongside* it rather than replacing it. See "The Bitboard Engine" and "Future
Work and Performance Outlook" below for what changed, what's still open, and
where performance is likely headed from here.

Overarching design philosophy (where item 1 is what I naturally got wrong before):

- Get the functionality right before worrying about performance or extensibility!!
- The solver is designed to be as simple and efficient as possible, with a focus on correctness and clarity.
- The solver is designed to be easily extensible, with a modular architecture that allows for easy addition of new features and optimizations.
- The solver is designed to be easily testable, with a comprehensive suite of unit tests and integration tests that cover all aspects of the solver's behavior.

---

## Architecture

### Key Classes

| Class | Package | Role |
|-------|---------|------|
| `Whist` | `bridge.cards` | Entry point; holds deal, opening leader, strain |
| `State` | `bridge.cards` | Bridge game state (whist, trick, tricks) |
| `Trick` | `bridge.cards` | Current trick state (plays, winner, leader) |
| `Tricks` | `bridge.cards` | Running trick totals (ns, ew) |
| `Deal` | `bridge.cards` | Four hands; card removal via `play` |
| `WhistState` | `bridge.gambit` | Gambit `State[State, State]` typeclass instance |
| `WhistGame` | `bridge.gambit` | Gambit `Game[State, CardPlay, Int]` typeclass instance |
| `AlphaBetaPlayer` | `gambit.game` | Generic minimax player (from Gambit) |

### Type Parameters for Bridge

```
P  = State       (proto-state and state are the same type)
S  = State       (bridge game state)
M  = CardPlay    (a single card play: hand, suit, rank)
Pl = Int         (partnership: 0=NS, 1=EW)
K  = (Long,Long,Long,Long)  (transposition table key, currently unused)
```

---

## Entry Point

```scala
def analyzeDoubleDummy(
                        tricks: Int,
                        directionNS: Boolean,
                        depth: Int = ...
): DDResult
```

- `tricks` — the number of tricks the protagonists need to make
- `directionNS` — `true` if NS are the protagonists (declarer side)
- Returns a `DDResult`:
    - `DDResult.Exact(makes)` — full search completed; result is definitive
    - `DDResult.Partial(makes)` — node limit hit, but one side found a witness line
    - `DDResult.Inconclusive` — node limit hit before either side found a witness

### DDResult

```scala
enum DDResult:
  case Exact(makes: Boolean)
  case Partial(makes: Boolean)
  case Inconclusive
```

`Exact` and `Partial` both carry a `makes: Boolean`. `Partial` is a qualified
best-effort: the search confirmed one side has a winning line, but the opposing
side's best response was not fully explored. `Inconclusive` means no single
top-level move completed within the node budget — no reliable conclusion can
be drawn. Callers should distinguish all three cases; `analyzeMakableContracts`
logs each result type distinctly.

Construction order in `analyzeDoubleDummy` is important: `gameTC` must be
declared as a named `given` before `stateTC` so that `WhistState` can find
it implicitly:

```scala
given gameTC: WhistGame = new WhistGame(this)
given stateTC: WhistState = WhistState(tricks, directionNS)
given com.phasmidsoftware.gambit.game.State[State, State] = stateTC
given com.phasmidsoftware.gambit.game.Game[State, CardPlay, Int] = gameTC
```

---

## WhistState — State Typeclass

`WhistState(neededTricks: Int, directionNS: Boolean)(using game: WhistGame)`
implements `GState[State, State]`.

### isGoal

Goal detection with early termination:

- `Some(true)` — protagonists have reached `neededTricks`
- `Some(false)` — opponents have blocked (decided), OR insufficient moves
  remain (`sufficientMovesRemaining` returns false)
- `None` — game still in progress

**Mid-trick guard**: `isGoal` only fires when `trick.isComplete || trick.size == 0`.
It must not fire mid-trick (size 1, 2, or 3) because the trick outcome is not
yet determined. Firing mid-trick would evaluate a partial state as terminal,
giving wrong results.

### heuristic

NS-absolute convention: positive = good for NS, negative = good for EW.

- `Some(true)` → `Double.MaxValue`
- `Some(false)` → `-Double.MaxValue`
- `None` → `State.heuristicFitness(s)` (tricks taken + potential)

### leafValue override

```scala
override def leafValue(s: State, maximizing: Boolean): Double = heuristic(s)
```

The default Gambit `leafValue` negates `heuristic` when `maximizing=false`
(negamax convention). Since bridge uses an NS-absolute heuristic, the negation
is incorrect — we override to always return `heuristic(s)` directly, regardless
of `maximizing`.

### isMaximizing override

```scala
override def isMaximizing(s: State, currentMaximizing: Boolean): Boolean =
  game.currentPlayer(s)(using this) == me
```

The default Gambit `isMaximizing` returns `!currentMaximizing` (strict
alternation). In bridge, the winner of a trick leads the next trick — the same
partnership can lead multiple tricks in a row. We override to compute the
current player from the actual game state rather than assuming alternation.

---

## WhistGame — Game Typeclass

`WhistGame(whist: Whist)` implements `Game[State, CardPlay, Int]`.

### currentPlayer

```scala
override def currentPlayer[P](s: State)(using state: GState[P, State]): Int =
  val seat = if s.trick.isComplete then
    s.trick.winner.map(_.play.hand).getOrElse(startingPlayer)
  else
    s.trick.leader.map(l => (l + s.trick.size) % 4).getOrElse(startingPlayer)
  if seat % 2 == 0 then 0 else 1 // normalize to partnership: 0=NS, 1=EW
```

Returns `0` (NS) or `1` (EW) — the partnership level, not individual seat.
This is correct because NS partners always play on the same side, and the
solver treats them as a single maximizing/minimizing entity.

### moves

Legal moves are derived from `State.enumeratePlays` — the successor states
reachable by each legal card play. The `CardPlay` for each successor is
recovered via `lastPlay`.

---

## Heuristic — State.heuristicFitness

The non-terminal heuristic estimates NS's trick-taking potential:

```
heuristicFitness(s) = deal.evaluate + trickBonus
```

- `deal.evaluate` — sum of NS holding strength minus EW holding strength
- `trickBonus` — partial credit for the current in-progress trick based on
  the current winning card, trick size, and whether it can still be beaten

The heuristic is always NS-absolute (positive = good for NS).

---

## Player Turn Order

Within a trick, play proceeds clockwise: seats 0 (N), 1 (E), 2 (S), 3 (W).
`currentPlayer` computes the next seat as `(leader + size) % 4` and normalizes
to partnership. This means within a single trick, `isMaximizing` alternates
between NS and EW as each card is played — the solver correctly interleaves
maximizing and minimizing within a trick.

---

## Transposition Table

The transposition table is **enabled** via `FlatTTCache` with key type
`CacheKey = (Long, Long, Long, Long)` — the four hands encoded as bitfields
of card sequences (using sequence priority rather than raw rank for ~3x
performance improvement).

```scala
given TTCache[CacheKey] = FlatTTCache()
val player = new AlphaBetaPlayer[State, State, CardPlay, Int, CacheKey](
  me = if directionNS then 0 else 1,
  depth = depth
).withMaxNodes(Whist.MAX_NODES)
 .withKeyFn(s => s.evaluateKey)
 .withAspirationWindow(AlphaBetaWindow(-0.5, 0.5))
```

Each entry stores a `TTFlag` (Exact / LowerBound / UpperBound). Currently
**only `Exact` entries are returned on probe** — LowerBound/UpperBound entries
are stored but not reused because doing so requires propagating tightened bounds
back to `cachedEvaluate`, which is deferred (Issue #14 partial).

In practice the `Exact`-only hit rate for bridge is low — most positions are
reached via paths with different alpha-beta windows, producing LowerBound or
UpperBound entries. The ~10,000-entry table on a 12-card ending gives negligible
speedup over `keyFn = None`. Full bound propagation is the primary remaining
performance lever from the TT side.

The previous `depthTranches` and `reuseDeeper` parameters have been removed;
the choice of caching strategy is now encoded in the `given TTCache[K]` instance.

---

## Aspiration Search

`analyzeDoubleDummy` asks a binary yes/no question: can the protagonists make
exactly `tricks` tricks? The terminal heuristic returns `+Double.MaxValue` for a
protagonist win and `-Double.MaxValue` for a loss, and the caller tests
`score > 0`.

Rather than searching from `[-∞, +∞]`, `withAspirationWindow(-0.5, 0.5)` is
applied to the player. The initial root window is now `(-0.5, 0.5)`:

- A fail-high (score ≥ 0.5) means the protagonists can make the contract —
  the search prunes the moment it confirms a win, without proving the exact margin.
- A fail-low (score ≤ -0.5) means they cannot — the search prunes the moment
  it confirms a loss.

Both sides fail fast. In practice this eliminates the majority of nodes that
plain alpha-beta would visit to establish the exact score, since the solver only
needs to cross zero rather than propagate the full `±Double.MaxValue` signal
back to the root.

The window values `(-0.5, 0.5)` are chosen to straddle zero with minimal
clearance. Because all terminal scores are either `+Double.MaxValue` or
`-Double.MaxValue` (never a fractional value), any score in `(-0.5, 0.5)` is
impossible at a terminal node — there is no risk of the window accidentally
cutting off a valid result.

---

## Early Termination

`sufficientMovesRemaining` prunes branches where the protagonists provably
cannot reach `neededTricks` even if they win all remaining tricks. This is
critical for performance — without it, the search explores branches that can
never achieve the goal, making full 52-card analysis intractable.

```scala
def sufficientMovesRemaining(moves: Int, directionNS: Boolean,
                              neededTricks: Int, tricks: Tricks): Boolean =
  val requiredMoves = (neededTricks - protagonistTricks) * 4
  moves >= requiredMoves || (declaringSideCanWin && plays >= requiredMoves - moves)
```

---

## Known Limitations and Open Issues

### Issue #14 — Transposition Table Flags (partial)

`TTFlag` (Exact/LowerBound/UpperBound), `TTEntry`, and the `TTCache[K]`
typeclass are implemented. `FlatTTCache` is wired into `analyzeDoubleDummy`.
Only `Exact` entries are currently reused on probe; full `LowerBound`/
`UpperBound` bound propagation is deferred.

In practice the Exact-only hit rate is too low to significantly accelerate
the large deals. Full bound propagation requires changing `probe` to return
`Option[TTEntry]` and handling window tightening in `cachedEvaluate`.

### Performance

Performance degrades exponentially with deal size. The 12-card ending takes
~1.5s with or without caching (Exact-only hit rate too low to help). The full
52-card deal is currently intractable within the node budget.

Suit-level grouping (treating equivalent cards as one move) is already
implemented and is the primary branching-factor reduction.

**Iterative deepening** (implemented) — `runPlayer` now calls
`chooseMoveIterativeDeepening` with `DEPTH_STEP = 4` (trick boundaries).
The search proceeds through depths 4, 8, 12, … 52, re-ordering top-level moves
at each iteration using the previous iteration's actual minimax scores. The node
budget is shared across all iterations. With 5M nodes, the search reaches
depth 28 (7 complete tricks) before the node budget is exhausted.

`DDResult` now carries a `tricks` field indicating how many complete tricks were
searched in the last completed iteration:

- `DDResult.Exact(makes, tricks)` — all iterations completed to `depth`
- `DDResult.Partial(makes, tricks)` — node limit hit; `tricks` = last completed depth / 4
- `DDResult.Inconclusive` — node limit hit before even depth-4 completed

The remaining performance levers are:

1. **Full TT bound propagation** (Issue #14)

### Memory

Profiling reveals ~250KB per 100K nodes (≈25 bytes/node average). The heap
grows almost monotonically — objects on the recursive call stack are retained
for the full depth of the search, preventing GC from collecting them.

The root cause is the immutable copy chain: every `State.create` call produces
a new `Whist` → `Deal` → 4×`Hand` → `Holding`/`Sequence` object graph. At
depth 52, all 52 levels of this chain are simultaneously live on the stack.

**`Hand.promote` short-circuit** (implemented): `Hand.promote` now returns
`this` when the hand is void in the led suit, avoiding a new `Hand` and `Map`
allocation for that case.

**`Holding.promote` short-circuit** (implemented): `Holding.promote` now
returns `this` when all sequences have priority ≤ the played priority — meaning
the played card ranks below all our sequences and no promotion is warranted.
Together with the `Hand.promote` void short-circuit, allocations are avoided
at two levels. Since hands become increasingly void and unaffected as the game
progresses, savings grow with search depth.

**`CardPlay` stores `Card` not `Deal`** (implemented): `CardPlay` previously
held a `Deal` reference to resolve display. It now stores the resolved `Card`
directly (just suit + rank), with the `Deal` used only at construction time
and immediately discarded. This is a cleaner design — historical play display
is correct regardless of subsequent deal evolution — though the memory saving
is modest since the `Deal` objects were already live on the stack via the
`State`/`Whist` chain.

**GC thrashing at depth 32+**: with a shared node budget, by the time
depth-28 completes the heap is nearly exhausted. The JVM spends most of its
time in GC rather than searching. The fix is a **per-iteration node budget**
(`NODES_PER_ITERATION = 1_000_000`) — resetting the node counter between
iterations allows GC to recover between depths, since the previous iteration's
stack has fully unwound and its objects are reclaimable. With this fix the
search reliably reaches 5-6 tricks depth and produces correct `Partial` results
for all 20 contracts of a full deal in ~360 seconds.

The next structural fix would be play/unplay (mutable `Deal` with undo on
backtrack), which would eliminate the copy chain entirely but requires
significant refactoring of `Deal`, `Hand`, and `Holding`.

### OOM / Node Limit

Analyzing all contracts for a deal (up to 20 strain × leader combinations) with
no node limit causes OOM. The node-count limit (`MAX_NODES`, tunable in `Whist`,
currently 5M) prevents this. The JVM heap should be configured to at least 8GB
via `.jvmopts` in the project root:

```
-Xms512m
-Xmx8g
```

Note: `javaOptions` in `build.sbt` requires `fork := true` and `runMain` (not
bare `run`) to take effect. The `.jvmopts` file is more reliable for the sbt
launcher JVM.

### Full Deal Analysis

With iterative deepening (`DEPTH_STEP = 4`), per-iteration node budget
(`NODES_PER_ITERATION = 1_000_000`), and 8GB heap, the solver produces correct
`Partial` results for all 20 contracts of a full 52-card deal in ~360 seconds.
Results reach 5-6 tricks depth consistently. All results agree with the known
double-dummy table in the PBN file.

The primary remaining bottleneck is the immutable `State`/`Deal` copy chain.
Play/unplay (mutable `Deal` with undo) would eliminate this and allow much
deeper search within the same memory budget.

The reference DDS implementation achieves full deal analysis in optimised C++;
a JVM Scala implementation will be slower but should be tractable with
play/unplay and correct TT flag reuse.

---

## Main Program

A standalone entry point is provided for running double-dummy analysis on PBN files:

```
com.phasmidsoftware.bridge.cards.doubleDummySolver
```

Usage (from the project root via sbt):

```
sbt "runMain com.phasmidsoftware.bridge.cards.doubleDummySolver path/to/file.pbn 0"
```

Arguments:
- `path/to/file.pbn` — path to a PBN file
- `0` — zero-based index of the game within the file to analyze

The program calls `analyzeMakableContracts` on the selected game, which evaluates
all contracts in the `OptimumResultTable` tag and prints a `DDResult` for each.
With the current settings (`NODES_PER_ITERATION = 1_000_000`, `DEPTH_STEP = 4`,
8GB heap) a full 20-contract deal takes ~360 seconds and returns `Partial(true, 5-6)`
for all contracts when given the double-dummy optimum trick count as input.

Note: the `.jvmopts` file in the project root must set `-Xmx8g` for the heap
to be available. `runMain` (not bare `run`) is required for `fork := true` to
take effect.

---

## Testing

### Three-Card Automatic Squeeze

The canonical test case is a three-card automatic squeeze:

- North: AJ♠, K♥
- East: KQ♠, A♥ (the squeezee)
- South: 2♠, 2♥, A♣ (the squeeze card)
- West: 98♠, 4♦ (inert)

At notrump (or a club contract), South leads A♣. North pitches K♥. East is squeezed:
- If East pitches Q♠: North's J♠ becomes good → NS makes 3
- If East pitches A♥: South's 2♥ becomes good → NS makes 3

Expected results: `DDResult.Exact(true)` only when South leads notrump or clubs
(`neededTricks=3`). All other leaders and all trump strains return
`DDResult.Exact(false)`.

### PBN Tests

Five deals from PBN (Portable Bridge Notation) files are tested against known
double-dummy results. These are larger deals requiring the full solver depth.
All are currently `pending` as they require iterative deepening to complete
within the node budget.

---

## Bugs Fixed During Development

Seven bugs were identified and fixed during the initial implementation:

1. **`isMaximizing`** — wrong player alternation assumption; bridge doesn't
   strictly alternate since trick winner leads next
2. **`leafValue`** — wrong sign convention at terminal nodes (negamax vs
   NS-absolute)
3. **`isGoal` mid-trick guard** — firing `Some(true/false)` mid-trick gave
   wrong results when the trick outcome was not yet determined
4. **Transposition table poisoning** — cached values from suboptimal lines
   suppressed correct results
5. **`currentPlayer` partnership vs seat** — returning individual seat (0-3)
   instead of partnership (0-1) caused wrong `maximizing` computation
6. **`_enumeratePlays` leader** — wrong leader selected after a complete trick
7. **`enumerateSubsequentPlays` leader** — related leader computation bug

---

## The Bitboard Engine

### Motivation

The object-graph engine's search algorithm (alpha-beta, iterative deepening,
a transposition table, an aspiration window — all from Gambit) is sound and
already reasonably tight. The actual cost, confirmed by reading the hot path
rather than guessing, was the state representation: `Deal.play` rebuilds all
four hands' persistent `Map`/`Seq` structures on every single card play, and
`WhistGame.moves`/`applyMove` build full child `State`s just to read off one
field, then discard them and re-derive the same candidates a second time in
`applyMove`. None of that is inherent to double-dummy search; it's an
artifact of representing a hand as a `Map` of `Holding`s of `Sequence`s.

The fix: represent the search state as bitmasks (one `Long` per hand, one
`Int` per suit) instead of the `Deal`/`Hand`/`Holding`/`Sequence` object
graph, and run it as a **second, independent engine alongside the first** —
not a rewrite of it. Every new result is cross-validated against the
object-graph engine's known-correct answer on the same deal and target before
being trusted (`BitAnalysisSpec`, `BitAnalysisITSpec`, `WinchesterBoard1Spec`,
`WinchesterBoard12Spec`). This project deliberately never got to the point of
touching `State`/`WhistGame`/`WhistState` themselves — the additive approach
was safer and, so far, sufficient to prove out the idea.

### Key Classes

| Class | Package | Role |
|-------|---------|------|
| `SuitMask` | `bridge.cards.bits` | Opaque `Int`; a 13-bit mask over one suit's ranks |
| `DealBits` | `bridge.cards.bits` | Four hands, each a `Long` (`suitIndex*13 + rank` per bit) |
| `BitState` | `bridge.cards.bits` | Search state: `DealBits` + strain + trick-in-progress + running tricks |
| `TrickBits` | `bridge.cards.bits` | Trick-winner scoring, ported from `Trick.score` to bit positions |
| `BitConversions` | `bridge.cards.bits` | `Deal ⇄ DealBits`, `Suit ⇄ Int` at the boundary (parsing/display only) |
| `BitWhistGame` | `bridge.gambit.bits` | Gambit `Game[BitState, TrickPlay, Int]` typeclass instance |
| `BitWhistState` | `bridge.gambit.bits` | Gambit `State[BitState, BitState]` typeclass instance |
| `BitAnalysis` | `bridge.gambit.bits` | Entry point, mirrors `Whist.analyzeDoubleDummy` |

`Card`/`Suit`/`Rank` and the `Holding`/`Hand`/`Deal` object graph are kept —
they still do the PBN-parsing and display work — but nothing on the search
hot path touches them; conversion happens once at the root and, for display,
once at the leaves.

### Design Decisions

**Bit direction is inverted from `Rank.priority`.** Bit 0 is the lowest
surviving rank, bit 12 the highest — the opposite of `Rank.priority`
elsewhere (where 0 is the Ace). This makes "does card X beat card Y" a plain
integer comparison of bit positions, and makes rank promotion free: once a
higher card's bit is cleared everywhere, the next card down is simply the new
highest set bit. No `Holding.promote`/`quit` bookkeeping is needed at all.

**Equivalence classes replace `Sequence`, computed fresh, not maintained.**
`SuitMask.equivalenceClasses(handBits, opponentBits)` partitions a hand's
suit-mask into maximal runs uninterrupted by an **opponent**-partnership
card — not any other hand's card. A partner's card sitting between two of the
hand's own cards does **not** break their equivalence; only a live opponent
card does. This one distinction — partner doesn't count, opponent does — is
the load-bearing design decision behind how aggressively the search prunes
its own move list, and is discussed further under "Future Work" below, in
response to a direct question about it.

**The transposition-table key was designed correctly the first time.**
`BitState.evaluateKey` packs the current trick's leader, in-progress plays,
and `tricks.ns` into the spare high bits (52–63) of each hand's `Long`. This
mirrors a fix that had to be **retrofitted twice** into the object-graph
engine's `State.evaluateKey` after two real collision bugs were found by
testing (see "Correctness Fixes" below) — the bit engine's key included both
from the start, precisely because that lesson was already learned once.

**Deliberate simplifications, chosen for a smaller and more easily verified
first implementation, not for correctness reasons** (documented in
`BitState`'s own class doc):

- No `Strategy`-based move ordering (`Cover`/`Duck`/`Finesse`/`Ruff`/etc. from
  the object-graph engine) — relies solely on Gambit's generic heuristic-based
  reordering. This offers *every* equivalence-class representative as a legal
  discard/ruff candidate, which is strictly more (never fewer) than the old
  engine tries — correct by construction, just not as tightly ordered.
- A weaker (but always-safe) "can the declaring side still reach the target"
  check — omits the object-graph engine's extra `declaringSideCanWin`
  tightening. Can only be slower to prove `false`, never wrong.
- A coarser non-terminal heuristic (tricks-banked-so-far only, not the
  object-graph engine's full card-potential `Deal.evaluate`). Only affects
  move-ordering quality and unproven `Partial` estimates — never a proven
  `Exact` result, since `isGoal` overrides the heuristic wherever it fires.

### Shared Configuration (`BridgeConfig` / `application.conf`)

Both engines now read the same tunables from
`src/main/resources/application.conf` via `BridgeConfig`, each overridable
without editing the file (e.g. `-Dbridge.nodes-per-iteration=2000000`):

- `bridge.transposition-table.max-size` (`ttMaxSize`, default 800,000) — caps
  `FlatTTCache` so a hard position can't grow the table without bound and OOM
  before the node budget is hit; a full table just means more cache misses,
  never a wrong answer.
- `bridge.nodes-per-iteration` (`nodesPerIteration`, default 1,000,000) — the
  per-iteration node budget for `withMaxNodes`; both `Whist.analyzeDoubleDummy`
  and `BitAnalysis.analyzeDoubleDummy` also accept an explicit `maxNodes`
  parameter overriding this default for a single call (used by
  `WinchesterBoard12Spec` to give the two engines very different budgets —
  see below).
- `bridge.aspiration-window` (`aspirationWindow`, default 0.5) — the one free
  parameter; `BridgeConfig.heuristicScale` (used by `BitState.heuristic`) is
  *derived* from it rather than configured independently, since the two must
  stay coupled for the aspiration-window technique to remain sound.

`build.sbt`'s `forwardedBridgeProps` forwards any `-Dbridge.*` system property
into the forked `Test`/`IT`/`run` JVMs, since `fork := true` would otherwise
swallow it silently.

### Gambit-Side Fixes (published as Gambit 1.2.2)

Three fixes to the shared search engine, independent of the bitboard work but
motivated by it:

- **`LowerBound`/`UpperBound` transposition entries are now reused as
  cutoffs**, not just `Exact` — the object-graph engine's own design doc
  above had flagged this as "the primary remaining performance lever from the
  TT side" (Issue #14); it's now done in `TTCache.scala`.
- **Aspiration-window fail re-search**: when a narrow-window search fails high
  or low, `searchWithAspirationRetry` now re-searches with the full window
  rather than trusting the narrow, unresolved value. Without this, a single
  heuristic overshoot at one node could get cached and reused verbatim at
  every other node reaching that same position, turning a one-off estimate
  into a repeated, amplified wrong answer.
- **The transposition table is cleared at the start of every
  iterative-deepening iteration**, not just once at the very start of the
  search — otherwise it silently accumulates entries computed under earlier
  iterations' now-stale alpha-beta windows.

### Correctness Fixes Along the Way

- **`evaluateKey` collision, found and fixed twice** in the object-graph
  engine: first, trick-in-progress state (leader/count/in-progress plays)
  was missing entirely from the key, so two genuinely different positions
  reached via different partial tricks hashed identically; then `tricks.ns`
  itself turned out to be a second, separate gap. Both are now included.
  `BitState.evaluateKey` was designed with both from day one.
- **`Trick.sufficientMovesRemaining`** fixed to not override an already-
  exhausted deal.
- **`BitWhistGame.currentPlayer`** was returning the raw hand index (0–3)
  instead of the partnership (0/1) in its first draft — the exact same class
  of bug the object-graph engine's design doc already documents fixing once
  ("`currentPlayer` partnership vs seat"), recurring in the new engine and
  caught only by cross-validating against the old one.

### Test Infrastructure

- The `IT` sbt configuration — present in `build.sbt` but commented out —
  was restored and wired to `src/it/scala`, so `sbt IT/test` (or
  `sbt "IT / test"`) actually runs it for the first time via a normal sbt
  task.
- Cross-validation specs were added comparing the two engines' live answers
  against each other (not just against a fixed hand-derived expectation) on
  both small hand-built end positions and real club/tournament deals
  (`BitAnalysisSpec`/`BitAnalysisITSpec`, `WinchesterBoard1Spec`,
  `WinchesterBoard12Spec`).
- The slowest of these — the full-52-card `pendingUntilFixed` Winchester
  board specs, and the ten-through-thirteen-card head-to-head cross-checks —
  were relocated into `IT` once they pushed the default `sbt test` run past
  7 minutes and, in one run inside IntelliJ (which does not necessarily
  apply the same `-Xmx8g` fork setting `build.sbt` gives the sbt-launched
  JVM), exhausted the heap. Default `sbt test` is back under a minute.
- Running the full `IT` suite for what appears to be the first time (per the
  above, it was never wired into sbt before this work) surfaced 25 failures
  in specs that predate this project (`ProblemSpec`, `WinchesterSpec`,
  `WhistPBNSpec`, `AnalysisSpec`). Initially parked deliberately rather than
  swept under the rug — since resolved; see "Real-Deal Integration Specs
  Rewritten" below for the two real bugs found and the full fix.

### Empirical Performance Finding

At 5× the default per-iteration node budget (5,000,000 nodes), the
object-graph engine reliably runs out of memory — observed: 13 minutes, heap
climbing steadily to an 8GB ceiling, dead mid-iteration. The bit engine
handles the identical 5× budget in **~14 seconds**, heap never exceeding
~2.6GB, with clear GC reclamation between checkpoints. This is a measured
result, not a design argument: the bit engine genuinely does not retain
`Deal`/`Hand`/`Holding` objects the way the object-graph engine does.

### Known Open Gap — Update, 2026-07-18: mostly resolved, and not for the reason first suspected

The ten/eleven/twelve-card synthetic end positions in `BitAnalysisITSpec`
originally all disagreed with the object-graph engine — the bit engine
converged to less depth and landed on a wrong `Partial` guess on all three.
Two rounds of investigation, in order:

**Round 1 — move ordering (see "Move Ordering", below).** Porting the
object-graph engine's `Strategy` system to the bit engine, as this section
originally predicted, fixed the ten-card case outright and measurably
improved depth elsewhere (`WinchesterBoard1Spec`'s "needing 2 tricks" case:
depth 5 → 7). It did **not** fix the eleven- or twelve-card cases — same wrong
answers, same depths, before and after.

**Round 2 — node budget and TT size (see "Bit-Engine-Specific Tuning",
below).** Both remaining cases turned out to be pure node-budget/TT-capacity
problems, not search-quality problems at all: given a big enough transposition
table *and* enough nodes, both resolve to a fully-proven `Exact` result that
agrees with the object-graph engine. The eleven-card case now resolves at the
project's actual (conservative) settings. The twelve-card case still doesn't
— it needs a bigger table/budget than was judged worth the memory margin (see
below) — but this is now a known, quantified, deliberately-accepted tradeoff,
not an open question about *why* it disagrees.

So: move ordering was a real, useful fix, but it was solving a different
problem than the one causing most of this particular gap. The original
hypothesis in this section ("no `Strategy`-based ordering... a hypothesis,
not a diagnosis") is a good example of a plausible-sounding explanation that
turned out to be only partially right — worth remembering next time a
performance gap shows up with an equally plausible-sounding single cause.

## Move Ordering

`BitState.legalPlays` now ports the object-graph engine's `Strategy` system
(`Cover`/`Duck`/`Finesse`/`Ruff`/`Discard`/`LeadTopOfSequence`/etc., in
`Holding`/`Trick`) as a move-ordering score — `leadScore`/`followSuitScore`/
`discardScore` — feeding Gambit's move-ordering machinery a pre-sorted
candidate list instead of an arbitrary one. This is **ordering only**: the
full set of legal equivalence-class representatives is still returned, just
reordered. Three pieces, added incrementally:

- **Initial port**: suit/rank preference for leads, a simplified cover/duck
  rule for following suit (no trick-position distinction yet), and a
  ruff-preferred/lowest-card-first rule for discards.
- **Trick-position distinction**: `followSuitScore` was refined to mirror
  `Holding.getStrategyForFollowingSuit`'s actual per-position logic — 2nd hand
  only tries to win if the led card is an honor or a real sequence is held
  (else always ducks, "second hand low"); 3rd hand ducks if partner's already
  winning, else plays to win outright with the highest card if the card to
  beat isn't an honor, or finesses (cheapest sufficient card) if it is; 4th
  hand always covers as cheaply as possible.
- **Discard suit selection**: `discardScore` gained a coarse "keep length
  with dummy/declarer" proxy — comparing this hand's remaining length in a
  candidate suit against the *other* partnership's (`DealBits.opponentMask`)
  — since the old engine's own `Strategy` system never modelled this either
  (`Hand.discardOrRuff` only ever compared candidates by rank).

**Deliberately not ported**: the object-graph engine's discard/ruff behaviour
actually *restricts* to a single (lowest) candidate per suit, not just
reorders — this engine still offers every equivalence-class representative,
just ordered worst-first, preserving the "strictly more, never fewer, legal
candidates" property discussed under "On Branching" below. Adopting that
restriction would be the first place this engine trades soundness for speed;
declined for the same reason as the 4-and-2 question.

**Measured effect** (see "Known Open Gap" above for the fuller story): fixed
the ten-card `BitAnalysisITSpec` case outright, improved `WinchesterBoard1Spec`
depth (5 → 7 tricks at the same node budget), did not move the eleven/twelve-
card cases (those turned out to be budget/TT-size problems, not ordering
problems). Two related move-ordering ideas were identified but not
implemented — see "Not Yet Implemented" below.

---

## Bit-Engine-Specific Tuning

### Transposition-table size and node budget

The bit engine now has its own, larger TT size and node budget
(`bridge.bitboard.transposition-table.max-size` = 3,000,000,
`bridge.bitboard.nodes-per-iteration` = 5,000,000 — `BridgeConfig.
bitboardTtMaxSize`/`bitboardNodesPerIteration`), separate from the
object-graph engine's unchanged, much smaller defaults (800,000 / 1,000,000),
which the object-graph engine cannot tolerate at these sizes.

These specific numbers were **chosen empirically, not increased on general
principle**. The investigation (prompted by a direct question: "we should be
able to handle a higher budget, since memory used per state is a lot less
than the object model") found something non-obvious: raising the node budget
*alone*, with the TT still capped at the object-graph engine's size, did
almost nothing — a full table stops caching, and the search thrashes without
transposition reuse no matter how many more nodes it's allowed. **TT size is
the actual lever; node budget only matters once the table is sized to
match.** Measurements:

| TT size | Node budget | Eleven-card case | Twelve-card case | Peak heap |
|---|---|---|---|---|
| 800,000 (default) | 10,000,000 | `Partial` (unproven, stuck) | `Partial` (unproven, stuck) | — |
| 3,000,000 | 5,000,000 | `Exact(true,11)` — proven | `Partial` (unproven) | ~2.4GB |
| 8,000,000 | 10,000,000 | `Exact(true,11)` | `Exact(true,12)` — proven | ~5.1GB |

The conservative row (3,000,000 / 5,000,000) was adopted as the actual
default: it resolves the eleven-card case fully and costs a comfortable
memory margin (~2.4GB of an 8GB heap). The more aggressive row resolves both
known cases, but at roughly double the memory (~5.1GB of 8GB) for the sake of
one specific known-hard case — judged not worth halving the remaining safety
margin for. This is a real, deliberate, documented tradeoff, not a compromise
made for lack of trying the bigger numbers.

### LRU eviction (Gambit)

Separately, `FlatTTCache` (Gambit) previously refused every new entry once
the table reached its size cap — whichever nodes happened to be computed
first in a search occupied the table for the rest of that iteration, even
though later entries (often deeper into a promising line, once move ordering
has done its job) are frequently more valuable to keep. It's now backed by
`java.util.LinkedHashMap` in access-order mode, evicting the
least-recently-touched entry on overflow instead. Zero correctness risk (the
TT is a pure cache — a miss just means recomputing, never a wrong answer),
verified via a dedicated eviction-order test. Directly relevant to the
conservative TT-size choice above: better eviction quality is what lets a
smaller table go further.

---

## Boxing and Allocation Fixes

Several rounds of JFR profiling (`jfr view hot-methods` / manually tallying
`jdk.ObjectAllocationSample` by class — the `allocation-by-class` view
doesn't work against IntelliJ's own recordings, but the raw event query
does) found the same shape of problem repeatedly: a generic tuple or
collection boxes a field that a dedicated case class wouldn't, at a point in
the code executed once per node or once per candidate move — cheap-looking
in isolation, expensive multiplied across the whole search tree.

- **`CacheKey`** (Bridge): was `(Long, Long, Long, Long)`; every transposition-
  table probe/store boxed all four fields. Now a dedicated case class.
  Measured: ~15.7s → ~12.5s on the same benchmark test.
- **`FlatTTCache`'s backing `HashMap`** (Gambit): started at default capacity
  and repeatedly resized toward its target size (`HashMap.growTable` was the
  single largest hot-methods entry, 11.76%). Now pre-sized up front when a
  real bound is given.
- **`orderedMoves`'s sort itself** (Gambit): `sortBy((_, next) =>
  state.heuristic(next))` doesn't cache its key function's result — the
  comparator recomputes the heuristic on every pairwise comparison, so
  sorting `b` successors cost O(b log b) heuristic evaluations, not O(b).
  Fixed by decorating each successor with its heuristic value once, up
  front, before sorting.
- **`orderedMoves`'s successor pairing** (Gambit): even after the fix above,
  the decorated `(M, S, Double)` tuple still boxed its `Double` field on
  every successor — computing a value once and boxing it once are different
  costs, and fixing the first doesn't fix the second. Replaced with a
  dedicated `ScoredSuccessor` case class.
- **`legalPlays`' equivalence-class pairing** (Bridge): `classesInSuit`
  returned `Seq[(TrickPlay, SuitMask)]` — `SuitMask` is an opaque `Int`, and
  a generic tuple boxes it regardless of its static type, on every
  equivalence class, at every node. Replaced with a dedicated `ScoredPlay`
  case class that also folds in the same once-not-per-comparison fix as
  `orderedMoves` (the class doc calls out both reasons explicitly).
  Follow-up fix in the same pass: `followSuitScore` was itself allocating a
  fresh `(Boolean, Boolean)` tuple per call via a match returning a pair;
  split into two independent values.

All verified via before/after re-profiles of the same test
(`WinchesterSpec`'s board 1), not just reasoned about: `boxToInteger`/
`boxToDouble` were the two hottest methods in the original profile (Robin's
own IntelliJ snapshot) and are gone entirely from the hot-methods list after
the `ScoredPlay` fix; a `Tuple3` then appeared (from `orderedMoves`, on
Gambit 1.2.4, which predated its own fix) and disappeared after
`ScoredSuccessor` landed. What remains — a small `Tuple2` — traces to
`orderedMoves`'s own final unwrap back to its declared `Seq[(M, S)]` return
type, an O(b) cost now (once per successor) rather than the O(b log b) it
was multiplied to before either fix. Both `maximizingSearch` and
`minimizingSearch` (its only callers) discard the move and use only the
successor state, suggesting that final unwrap — and the move field
entirely — might not be needed at all; not pursued, flagged as a possible
further simplification needing its own verification.

---

## Real-Deal Integration Specs Rewritten

`AnalysisSpec`, `WhistPBNSpec`, `ProblemSpec`, and `WinchesterSpec` — the
four specs that produced 25 failures the first time the `IT` suite was ever
run (see "Test Infrastructure" above) — were rewritten to call `BitAnalysis`
instead of `Whist`, since none of them had ever been exercised against the
bit engine, and the old engine visibly struggles on real full deals anyway.

**Two real, pre-existing bugs found and fixed along the way, unrelated to
either engine's search quality:**

- `WhistPBNSpec` asserted `expected = false` on every declarer/strain/trick
  combination checked, even though `tricks` is parsed directly from
  `OptimumResultTable` — i.e. it IS the documented double-dummy optimum, so
  the correct expectation was always `true` (matching the other three
  specs' identical convention for the same kind of check). This alone
  explains most of that file's original failures.
- `ProblemSpec`'s three "modes" computed a `reuse`/`depthTranches` pair that
  was printed but never actually passed to `analyzeDoubleDummy` — dead
  parameters from the `depthTranches`/`reuseDeeper` API this document's
  Transposition Table section already noted as removed. All three modes
  tested the exact same thing three times over; collapsed to one test.

**Exact vs. Partial, properly distinguished.** All four specs now use
`assertProvenMakes` + `pendingUntilFixed` (the same pattern already
established by `WinchesterBoard1Spec`/`WinchesterBoard12Spec`) instead of the
original `assertMakes`, which treated an unproven `Partial` guess the same as
a proven `Exact` result for pass/fail purposes. On a full 52-card deal,
neither engine gets remotely close to a proof within a realistic budget
(observed depths as shallow as 6 of 52 plies) — grading an unproven guess as
a failure was testing a question neither engine could actually answer yet,
independent of which engine was used.

**Fixture corrections** (Robin's own work, not code): the Westwood fixture
was spot-checked and confirmed correct (first four deals); the Winchester
fixture had several wrong `OptimumResultTable` entries, corrected, with the
file trimmed down to exactly the five boards actually tested (1, 2, 3, 7,
12) — which shifted `WinchesterSpec`'s hardcoded board-7/12 array indices and
had to be fixed alongside the trim; LEXINGTON's entries were also found
wrong and corrected.

**Result**: with corrected ground truth and the Exact/Partial fix, Winchester
boards 1 and 3 now fully resolve to `Exact(true,13)` — a genuine, complete,
proven double-dummy result on a real deal, the first anywhere in this
project outside a small synthetic endgame. Boards 2, 7, and 12 (including
one, board 7, whose unproven guess is currently wrong) remain honestly
`Partial`/pending — not failures, since they were never proven either way.
Westwood (9 deals) and the corrected LEXINGTON (8 deals) are all still
`Partial`/pending at current settings — harder for the search to resolve
within budget than Winchester's, apparently, not a bug.

---

## Attempted and Reverted: a Richer Non-Terminal Heuristic

`BitState.heuristic` was, until this attempt, purely tricks-banked-so-far —
the "deliberate simplification" this document already flagged, since a
proven `Exact` result is never affected by the heuristic (only move-ordering
quality and unproven `Partial` estimates are). A bit-native port of the
object-graph engine's genuine card-potential evaluation (`Deal.evaluate`/
`Holding.evaluate` — each equivalence class contributes `size * 0.5^priority`,
`priority` being how many still-live cards outrank its top card) was
implemented, with a rigorously-bounded scale derivation (worst case 13 tricks
+ 26 card-potential = 39, replacing the trick-count-only bound of 13) to keep
it safe for the aspiration window.

**Reverted the same day.** Verified correctness-neutral, but empirically:
zero change to the eleven/twelve-card gap (identical wrong answers, identical
depths — this was before the budget/TT-size root cause above was found), and
a severe slowdown (an isolated benchmark that normally takes ~11s didn't
finish in 5+ minutes and had to be killed). Root cause: at the time, Gambit's
`orderedMoves` still recomputed the heuristic once per comparison during
sorting (see "Boxing and Allocation Fixes" above) — an O(b log b) multiplier
on top of a heuristic that was already far more expensive than the O(1)
trick-count subtraction it replaced. The code is preserved, unwired, in
`CardPotentialHeuristic.scala`, clearly labelled, in case it's worth
revisiting.

**Worth noting explicitly**: the recomputation bug this heuristic ran into
is now fixed (both the O(b log b)-calls problem and the Tuple boxing
problem, see above) — meaning a retry today would not pay that multiplier.
The heuristic's own per-call cost (equivalence-class walks across four
suits, four hands) would still need to be reduced or the results would still
likely be too slow, but the gap between "hopeless" and "worth trying" is
probably smaller now than when it was first attempted. See "Not Yet
Implemented" below.

---

## Future Work and Performance Outlook

### Not Yet Implemented

Status as of 2026-07-18. Estimates below are deliberately calibrated, not
optimistic — most are ranges reflecting real uncertainty, not point figures,
since none of this has been prototyped. Where a prior estimate in this
document turned out to be only partly right (see "Known Open Gap" above),
that's a reminder to treat these the same way: directional guidance, not
commitments.

1. **Rank reduction / cross-position suit canonicalization.** A suit's
   *relative* structure (which of the 13 ranks are alive and who holds them,
   independent of the absolute ranks) is often shared across otherwise
   distinct positions. Real double-dummy solvers (DDS/GIB) exploit this
   heavily; this codebase doesn't yet, beyond the exact-position
   transposition table. Scoped in the original project plan as a stretch
   goal; not started.
   **Estimated saving**: potentially the largest of anything on this list,
   and the one item that could change the *shape* of the performance curve
   rather than just its constant factor — real solvers reach sub-second full
   deals partly through this technique. Honestly speculative without a
   prototype: plausibly anywhere from a small constant-factor win (if few
   positions in a typical search actually share structure) to an
   order-of-magnitude-plus reduction in effective node count on hard
   positions (if sharing is common, as DDS/GIB's results suggest). Also the
   biggest engineering lift of anything here — a genuinely new project, not
   a tuning pass.
2. **Ruff rank-selection heuristic** (`discardScore` currently always prefers
   the lowest trump, with no model of being over-ruffed, promoting a
   partner's or opponent's trump honor, or setting up a second ruff).
   Ordering-only, same safety profile as the move-ordering work already
   done.
   **Estimated saving**: smaller and narrower than the move-ordering work
   already landed — it only affects positions where ruffing choices actually
   matter (trump contracts with more than one live trump to choose from),
   not every position. Plausibly a modest, single-digit-percentage reduction
   in node count on ruffing-heavy hands; unlikely to close a specific known
   gap the way the trick-position fix closed the ten-card case, since
   nothing currently flags a gap traceable to this specifically.
3. **Discard suit-selection heuristic — done** (the "keep length with
   dummy/declarer" proxy, see "Move Ordering" above). Listed here in the
   original version of this document as future work; implemented since.
4. **Revisit the reverted card-potential heuristic** (see "Attempted and
   Reverted" above) now that the O(b log b) recomputation bug it collided
   with is fixed. Would need the heuristic's own per-call cost reduced too
   (e.g. precompute each suit's live-card mask once per node and share it
   across all four hands, rather than recomputing it once per hand as the
   reverted version did) before it's worth re-testing.
   **Estimated saving**: unknown in either direction until retried — this is
   explicitly a "worth checking again, circumstances changed" item, not a
   confident prediction. If it works, the upside is the same as originally
   intended: better move ordering across the board, and more accurate
   (though still unproven) `Partial` guesses on hard positions — plausibly
   relevant to the still-open twelve-card gap and the still-`Partial`
   Westwood/LEXINGTON real deals. If it still doesn't pay for its own cost
   even after both fixes, that's a fast, cheap experiment to run (re-wire it,
   re-profile, compare), not a big investment.
5. **Parallel/multi-threaded search.** Not attempted, not scoped at all.
   **Estimated saving**: a well-implemented shared-TT parallel search (e.g.
   Lazy-SMP, as used in strong chess engines) could plausibly give a
   near-linear win in *usable node budget per unit wall-clock time* with
   core count on a modern multi-core machine — e.g. roughly 3-6x on a
   typical 4-8 core machine, allowing for synchronization/TT-contention
   overhead eating into the naive linear number. Entirely orthogonal to
   every other item here (stacks multiplicatively with rank reduction, a
   cheaper heuristic, etc.), but a substantial, separate engineering effort
   (thread-safe `TTCache`, work-splitting, no existing scaffolding at all).
6. **`orderedMoves`'s final tuple unwrap** (see "Boxing and Allocation
   Fixes" above) — both its only callers discard the move and use only the
   successor state, suggesting the unwrap-to-`(M,S)` step, and possibly the
   move field of `ScoredSuccessor` entirely, isn't needed.
   **Estimated saving**: small — this is now an O(b), not O(b log b), cost,
   so the remaining upside is bounded to whatever fraction of allocation
   this one `Tuple2` per successor represents (a few percent at most, based
   on the last profile). Cheap to verify if ever done; not worth doing
   speculatively.
7. **The 25 original IT-suite failures — resolved**, not just investigated:
   all four specs rewritten to test the bit engine with a proper
   Exact/Partial distinction (see "Real-Deal Integration Specs Rewritten"
   above), two real bugs fixed, fixtures corrected. Listed here in the
   original version of this document as still-open; closed since.
8. **The object-graph engine's own "next structural fix"** (mutable
   `Deal`/play-unplay, noted in this document's original "Memory" section)
   remains effectively moot: the bit engine already solves that exact
   allocation problem via a different representation.

### Performance Prediction

The honest answer is still that sub-second full-52-card analysis is not
close. What's changed since this section was first written is that the
predicted quick win (move ordering) has actually been tried, and it
partially confirmed and partially corrected the original prediction: it did
shrink proof time on some hard positions as expected (the ten-card
`BitAnalysisITSpec` case, `WinchesterBoard1Spec`'s depth improvement), but it
did **not** close the eleven/twelve-card gap the way this section predicted
— that turned out to be a node-budget/TT-size problem, not a move-ordering
problem (see "Known Open Gap"). Both things can be true at once: move
ordering was worth doing, and it wasn't the single fix this section implied
it might be.

What's actually closed the ten/eleven-card gap and produced the first two
fully-proven real-deal results (Winchester boards 1 and 3) is a combination
of three things together — move ordering, a bit-engine-specific TT size/node
budget chosen empirically, and the LRU eviction and boxing fixes that made a
given memory budget go further. None of these change the underlying
exponential blow-up with deal size; they're all constant-factor (if a large
one — the memory/time comparison for the bit engine vs. the object-graph
engine is roughly two orders of magnitude) or capacity (bigger effective TT)
improvements, not structural ones.

Rank reduction and/or parallel search remain the candidates for something
structural — see their entries in "Not Yet Implemented" above for calibrated
(and explicitly uncertain) estimates. Absent those, getting a full 52-card
deal to `Exact` in real time is not expected soon: the twelve-card synthetic
case already needs roughly double the memory margin this project judged
worth spending, and a real deal is a much harder search than any of the
synthetic end positions tested so far.

### On Branching: the 4-and-2 Question

The question was: even when two cards in a suit haven't merged into one
`Sequence`/equivalence class, they're *usually* — but not always —
interchangeable in practice. Should the search treat them as one anyway?

Precisely stated, today's rule is: two cards in one hand are equivalent right
now if and only if no **opponent** card (partner's cards don't count) has a
rank between them. Holding the 4 and 2 of a suit with an opponent's live 3
between them is exactly the case the current rule does *not* merge — by
design, and correctly so under the strict definition, because in principle
either of the two remaining cards could later matter (which one is kept back
can affect a later cover/discard/entry decision once that opponent's 3 is in
play). This rule is **sound**: applying it can never change the true
double-dummy answer, it only ever avoids exploring a genuinely redundant
branch. Every optimization implemented in this project so far — bitboards,
this equivalence rule, the transposition table, the aspiration window,
iterative deepening — shares that property: none of them can turn a correct
answer into a wrong one.

Treating "usually but not always" as prune-and-move-on would be a genuinely
different kind of technique — heuristic forward pruning, as used in strong
chess/Go engines, which accepts a small bounded risk of missing the true best
line in exchange for speed. It is a real, known idea, but it would be the
first departure in this whole project from "every result is either proven or
honestly labeled unproven" — every prior fix here (the `evaluateKey`
collisions, the `currentPlayer` bug, refusing to paper over the OOM with a
bigger heap) has gone the opposite direction, toward *more* rigor, not less.

Recommendation: get the same practical benefit without the risk, by using
the intuition as **move ordering, not pruning** — try the "probably
equivalent" card first at each node (this is exactly what the `Strategy`
system already does in the object-graph engine — `Cover`/`Finesse`/
`LeadTopOfSequence`/etc. are precisely encoded "usually right" preferences,
expressed as an ordering, never as a legality restriction). If the guess is
right, as it usually is, alpha-beta prunes the sibling branches away for
free, at zero correctness cost. If it's occasionally wrong, the search just
does the same work it would do today — nothing is lost.

**Update**: this recommendation has since been implemented (see "Move
Ordering" above) — `leadScore`/`followSuitScore`/`discardScore` are exactly
this, "usually right" preferences expressed as ordering, never as legality.
It measurably helped (the ten-card case, `WinchesterBoard1Spec`'s depth), but
it did *not* turn out to be the same missing piece behind the eleven/twelve-
card gap, which the original version of this paragraph predicted — that gap
was a node-budget/TT-size problem instead (see "Known Open Gap"). The
recommendation itself — capture "usually right" as ordering, not pruning —
still stands regardless; it was the specific prediction about which gap it
would close that was only partly right.

A true forward-pruning mode is worth keeping in mind as a distinct, explicit,
opt-in future option (e.g. a new `DDResult` variant that's clearly labeled
heuristic, never conflated with `Exact`) if a use case ever needs speed more
than a guarantee — but it shouldn't be adopted silently as a change to the
existing search.