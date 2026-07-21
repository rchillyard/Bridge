# Bridge

A Scala 3 toolkit for contract bridge: a double-dummy solver (can a contract be
made, given perfect information?) and a duplicate-session scoring/directing
tool (matchpoints from real club recap sheets). It's an application, not a
published library — the code lives here on GitHub for reference and for use
directly from source.

## Double-dummy analysis

"Double dummy" means analyzing a deal with all four hands visible — the
standard way of asking "can this contract be made against best defense?"
There are two independent solver engines, kept side by side rather than one
replacing the other:

- **`Whist`** (`bridge.cards`) — the original engine: an immutable
  `Deal`/`Hand`/`Holding`/`Sequence` object graph, with real bridge tactics
  (`Cover`/`Duck`/`Finesse`/`Ruff`/`LeadTopOfSequence`, etc. in `Strategy`)
  encoded as move-ordering hints.
- **`BitAnalysis`** (`bridge.cards.bits` / `bridge.gambit.bits`) — a second,
  bitboard-based engine (each hand packed into a `Long`) built to eliminate the
  first engine's biggest cost (rebuilding the object graph on every card
  play), validated against `Whist`'s known-correct results rather than
  replacing it outright. It's the actively-developed one, and it's where an
  opening-lead priority scale lives — singleton leads, leading toward a
  partnership tenace, cashing to avoid a blocked suit, trump leads to strip
  ruffing potential — translated from real bridge tactics for use as
  move-ordering in a perfect-information search.

Both sit on [Gambit](https://github.com/rchillyard/Gambit), a separate
generic alpha-beta/transposition-table search library, and both parse PBN
(Portable Bridge Notation) files via `bridge.pbn`.

Run the solver against a PBN file:

```
sbt "runMain com.phasmidsoftware.bridge.cards.doubleDummySolver path/to/file.pbn 0"
```

(`0` is the zero-based index of the game to analyze within the file.) A
result is one of `Exact` (fully proven), `Partial` (a best-effort guess within
the node budget, not proven), or `Inconclusive`.

The full design history — why two engines exist, every performance and
correctness fix made along the way with its actual measurements, and what's
still open — is in [`doc/DoubleDummyDesign.md`](doc/DoubleDummyDesign.md).
That document is the real source of truth for this part of the project; this
README is deliberately just an orientation.

### Why alpha-beta, not Monte Carlo Tree Search

Both engines use alpha-beta minimax (via Gambit), not Monte Carlo Tree Search
(MCTS), an approach an older version of this project explored. The two solve
different problems:

- **Alpha-beta** is exhaustive and deterministic: it explores the full game
  tree, pruning a branch only once it's *proven* the branch can't affect the
  outcome. Given enough resources it terminates with a provably exact
  answer, never just an estimate — and its performance depends heavily on
  move ordering, since with the best possible ordering it needs roughly the
  square root of the naive branching factor's node count for a given depth.
- **MCTS** is statistical and incremental: it grows the tree asymmetrically,
  guided by random (or learned) playouts to completion, balancing
  exploration of under-tried moves against exploitation of ones that look
  good so far (typically via UCB1). It needs no hand-crafted evaluation
  function — the playouts are the signal — and can be stopped at any time
  with a "best guess so far" that keeps improving, but that guess is always
  a statistical estimate, never a proof.

Double-dummy analysis is perfect information with a modest branching factor
once equivalence classes collapse interchangeable cards together, and the
entire point is a *proven* answer against best defense — exactly alpha-beta's
strength. MCTS earns its keep where the branching factor is enormous and no
good evaluation function exists (Go is the classic case); bridge has neither
problem, so it was the wrong tool for this particular job.

## Duplicate session scoring

`bridge.director` scores a real duplicate bridge session from a club's recap
sheet (event/section/pairs/travelers), computing matchpoints and percentages.
This isn't a toy — it's been run against real club data (Kerem Shalom,
Newton, Lexington; see `src/test/resources/com/phasmidsoftware/bridge/director`
for examples).

```
sbt "runMain com.phasmidsoftware.bridge.director.Score path/to/recap-file"
```

## Building and testing

```
sbt compile          # main sources
sbt test             # fast unit tests (should complete in well under a minute)
sbt IT/test          # slower integration tests: real deals, larger synthetic
                      # end positions, and cross-validation between the two
                      # double-dummy engines -- can take from minutes to hours
```

The `.jvmopts` file sets an 8GB heap, needed for the harder double-dummy
searches; `IT/test` forks its own JVM with the same setting.

## Layout

| Package | Contents |
|---|---|
| `bridge.cards` | The object-graph double-dummy engine (`Whist`, `Deal`, `Hand`, `Holding`, `Strategy`, ...) |
| `bridge.cards.bits` | The bitboard double-dummy engine (`BitState`, `DealBits`, `SuitMask`, ...) |
| `bridge.gambit` / `bridge.gambit.bits` | Gambit typeclass wiring for each engine |
| `bridge.pbn` | PBN file parsing |
| `bridge.director` | Duplicate-session scoring from recap sheets |

## Status

Version 1.1.6. The double-dummy solver's known synthetic test positions (up
to thirteen cards per hand) are now fully resolved by the bitboard engine at
its default settings; full 52-card real deals are not, and aren't expected to
be without further structural work (see `doc/DoubleDummyDesign.md`'s "Future
Work and Performance Outlook"). This is presently paused, by design, rather
than abandoned.
