# Bridge Double-Dummy Solver — Design Document

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
  `WhistPBNSpec`, `AnalysisSpec`) — some match previously-flagged
  known-bad cases (e.g. a suspected fixture error on Winchester board 1;
  boards 3/7 already flagged as slow/non-terminating), others (`AnalysisSpec`/
  `WhistPBNSpec` failing on nearly every deal) don't have a prior baseline to
  compare against, since this command could not run before. Not yet
  investigated further — parked deliberately, not swept under the rug.

### Empirical Performance Finding

At 5× the default per-iteration node budget (5,000,000 nodes), the
object-graph engine reliably runs out of memory — observed: 13 minutes, heap
climbing steadily to an 8GB ceiling, dead mid-iteration. The bit engine
handles the identical 5× budget in **~14 seconds**, heap never exceeding
~2.6GB, with clear GC reclamation between checkpoints. This is a measured
result, not a design argument: the bit engine genuinely does not retain
`Deal`/`Hand`/`Holding` objects the way the object-graph engine does.

### Known Open Gap

On the ten/eleven/twelve-card synthetic end positions in `BitAnalysisITSpec`,
the bit engine converges to *less* search depth than the object-graph engine
on the identical position and target — a `Partial` result where the old
engine reaches `Exact`, or a shallower `Partial` than the old engine's. Not
yet root-caused. The leading suspect is the deliberate simplification noted
above: no `Strategy`-based move ordering, so the bit engine's search explores
its (equally legal) candidate moves in a less efficient order and does more
work to reach the same conclusion. This is a hypothesis, not a diagnosis.

---

## Future Work and Performance Outlook

### Not Yet Implemented

1. **`Strategy`-based move ordering for the bit engine.** The single most
   likely candidate to close the known depth gap above, and — see the next
   section — also the safe way to capture the branching-factor intuition
   behind the question that prompted this write-up. Purely a heuristic
   (affects ordering, not legality), so it carries no correctness risk.
2. **Rank reduction / cross-position suit canonicalization.** A suit's
   *relative* structure (which of the 13 ranks are alive and who holds them,
   independent of the absolute ranks) is often shared across otherwise
   distinct positions. Real double-dummy solvers (DDS/GIB) exploit this
   heavily; this codebase doesn't yet, beyond the exact-position transposition
   table. Scoped in the original project plan as a stretch goal; not started.
3. **Systematic profiling.** Every optimization so far has come from reading
   the hot path and reasoning about allocation and search structure, not from
   a profiler run (JFR/async-profiler) against the actual bit engine. Cheap
   to do, and would settle open questions like how much time now goes to TT
   hashing, GC, or recomputing equivalence classes fresh at every node versus
   caching them once per unique 26-bit opponent/own-suit combination.
4. **Parallel/multi-threaded search.** Not attempted at all. A modern
   multi-core machine could plausibly give a near-linear win via a shared-TT
   parallel search (e.g. Lazy-SMP style, as used in strong chess engines),
   entirely orthogonal to every optimization above.
5. **Root-causing the known gaps above** — the ten/eleven/twelve-card
   depth disagreement, and the 25 IT-suite failures in pre-existing specs —
   both flagged, neither investigated.
6. **The object-graph engine's own "next structural fix"** (mutable
   `Deal`/play-unplay, noted in this doc's original "Memory" section) is now
   effectively moot: the bit engine already solves that exact allocation
   problem via a different representation, so there's no remaining reason to
   pursue it in the old engine.

### Performance Prediction

The honest answer is that sub-second full-52-card analysis is not close, and
there is no single remaining change likely to get there — DDS/GIB reach that
speed through years of accumulated technique (rank reduction across all four
suits at once, a tuned move-ordering system, and in modern versions parallel
search plus small-suit-combination lookup tables), of which this project has
so far implemented one piece (equivalence classes) well, plus the standard
alpha-beta/TT/aspiration-window/iterative-deepening machinery. What's been
measured is a large **constant-factor** win — the memory/time comparison
above is roughly two orders of magnitude — not a change in the underlying
exponential blow-up with deal size.

A calibrated expectation, not a promise: restoring move ordering (item 1
above) should noticeably shrink the time to *prove* (`Exact`, not `Partial`)
harder end positions — plausibly from minutes down to single-digit seconds
for the currently-problematic ten-to-thirteen-card cases — because alpha-beta
efficiency is dominated by whether the first move tried at each node is
already the best one. Getting a full 52-card deal to `Exact` in real time
almost certainly still needs rank reduction and/or parallel search on top of
that; that's a project of comparable size to what's already been done here,
not a tuning pass.

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
does the same work it would do today — nothing is lost. This is also,
concretely, item 1 in "Not Yet Implemented" above: porting `Strategy`-based
ordering to the bit engine would likely address this concern and the known
ten/eleven/twelve-card depth gap at the same time, since both stem from the
same missing piece.

A true forward-pruning mode is worth keeping in mind as a distinct, explicit,
opt-in future option (e.g. a new `DDResult` variant that's clearly labeled
heuristic, never conflated with `Exact`) if a use case ever needs speed more
than a guarantee — but it shouldn't be adopted silently as a change to the
existing search.