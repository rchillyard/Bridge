# Bridge Double-Dummy Solver — Design Document

## Overview

This document describes the design of the double-dummy solver for the Bridge
project, which uses the Gambit framework (`AlphaBetaPlayer`) to perform
exhaustive minimax search over all possible card plays, with both sides assumed
to play perfectly with full knowledge of all four hands.

The solver is integrated via the Whist model — a simplified bridge game without
auction — and is accessed through `Whist.analyzeDoubleDummy`.

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
  depth: Int = ...,
  reuseDeeper: Boolean = false,
  depthTranches: Boolean = true
): Option[Boolean]
```

- `tricks` — the number of tricks the protagonists need to make
- `directionNS` — `true` if NS are the protagonists (declarer side)
- Returns `Some(true)` if the protagonists can guarantee `tricks` tricks
  against best defence, `Some(false)` if they cannot, `None` if no move
  is available

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

The transposition table is currently **disabled** (`keyFn = None`).

The naive transposition table (storing a `Double` per board position) produces
incorrect results because a value cached under one set of alpha-beta bounds may
be reused in a context with incompatible bounds, poisoning the search. This was
observed in practice: the correct winning line for NS was found but suppressed
because a cached `-MaxValue` from a suboptimal line was returned for the same
board position reached via a different path.

**Issue #14** tracks the correct fix: store TT flags (exact / lower bound /
upper bound) with each cached entry, and only reuse entries whose flag is
compatible with the current alpha-beta window (`alpha`, `beta`):

- `EXACT` — value is exact; reuse freely
- `LOWERBOUND` — value ≥ actual; only reuse if `value > alpha`
- `UPPERBOUND` — value ≤ actual; only reuse if `value < beta`

The transposition table key is `(Long, Long, Long, Long)` — the four hands
encoded as bitfields of card sequences (using sequence priority rather than
raw rank for ~3x performance improvement).

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

### Issue #14 — Transposition Table Flags

The transposition table is disabled pending implementation of exact/lower/upper
bound flags. Re-enabling it correctly will significantly improve performance for
large deals (the full Lexington hand currently takes ~10s without caching).

### Performance

Without the transposition table, performance degrades exponentially with deal
size. The `depthTranches` mode (separate cache per depth) was benchmarked at
~25% faster than flat for bridge when caching was enabled.

### Full Deal Analysis

The 52-card full deal analysis is the ultimate goal. Performance depends heavily
on the transposition table and the `sufficientMovesRemaining` pruning. The
`maxTableSize` parameter caps the transposition table at 800,000 entries to
avoid OOM.

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

Expected results: `Some(true)` only when South leads notrump or clubs (`neededTricks=3`).
All other leaders and all trump strains return `Some(false)`.

### PBN Tests

Five deals from PBN (Portable Bridge Notation) files are tested against known
double-dummy results. These are larger deals requiring the full solver depth.

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