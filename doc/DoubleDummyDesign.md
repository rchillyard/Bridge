# Bridge Double-Dummy Solver — Design Document

## Overview

This document describes the design of the double-dummy solver for the Bridge
project, which uses the Gambit framework (`AlphaBetaPlayer`) to perform
exhaustive minimax search over all possible card plays, with both sides assumed
to play perfectly with full knowledge of all four hands.

The solver is integrated via the Whist model — a simplified bridge game without
auction — and is accessed through `Whist.analyzeDoubleDummy`.

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

1. **Per-iteration node budget** — reset the node counter between iterations
   so GC can recover between depths; prevents GC thrashing at depth 32+
2. **Full TT bound propagation** (Issue #14)
3. **`Holding.promote` short-circuit** — return `this` when no sequence has
   priority ≥ the played priority (non-void but unaffected case)

### Memory

Profiling reveals ~250KB per 100K nodes (≈25 bytes/node average). The heap
grows almost monotonically — objects on the recursive call stack are retained
for the full depth of the search, preventing GC from collecting them.

The root cause is the immutable copy chain: every `State.create` call produces
a new `Whist` → `Deal` → 4×`Hand` → `Holding`/`Sequence` object graph. At
depth 52, all 52 levels of this chain are simultaneously live on the stack.

**`Hand.promote` short-circuit** (implemented): `Hand.promote` now returns
`this` when the hand is void in the led suit, avoiding a new `Hand` and `Map`
allocation for that case. Since hands become increasingly void as the game
progresses, savings grow with search depth — exactly where pressure is worst.
This produced measurable speedup on the 11- and 12-card end-position tests.

**GC thrashing at depth 32+**: with a shared 5M node budget, by the time
depth-28 completes the heap is nearly exhausted. The JVM spends most of its
time in GC rather than searching, making progress toward depth-32 extremely
slow. The fix is a **per-iteration node budget** — resetting the node counter
between iterations allows the GC to recover between depths, since the previous
iteration's stack has fully unwound and its objects are reclaimable.

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

The 52-card full deal analysis is the ultimate goal. With iterative deepening
and 5M nodes, the search reaches depth 28 (7 tricks) before GC pressure halts
progress. The next step is a **per-iteration node budget** — a smaller per-iteration
limit (e.g. 500K nodes) allows GC to recover between iterations, trading raw
depth for reliable progress through more iterations.

The reference DDS implementation achieves full deal analysis in optimised C++;
a JVM Scala implementation will be slower but should be tractable with
per-iteration budgets and correct TT flag reuse.

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