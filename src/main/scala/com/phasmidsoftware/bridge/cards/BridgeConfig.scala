/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.typesafe.config.ConfigFactory

/**
  * Runtime-configurable tuning values, backed by `application.conf` (see
  * `src/main/resources/application.conf`). `ConfigFactory.load()` already lets any of
  * these be overridden without touching the file, e.g.
  * `-Dbridge.transposition-table.max-size=1500000`.
  */
object BridgeConfig:
  private val config = ConfigFactory.load()

  /**
    * Maximum number of entries a double-dummy search's transposition table will hold
    * before it stops accepting new ones (`FlatTTCache.maxSize`). Bounds heap usage on
    * hard positions that would otherwise grow the table without limit; once full, a
    * store is simply a no-op (a cache miss later, not an error) -- this only ever costs
    * some redundant re-computation, never correctness.
    */
  val ttMaxSize: Int = config.getInt("bridge.transposition-table.max-size")

  /**
    * The node budget for a single iterative-deepening iteration (`AlphaBetaPlayer.withMaxNodes`).
    * The counter (and, separately, the transposition table) resets at the start of every
    * iteration, so this is a per-depth budget, not a total one.
    */
  val nodesPerIteration: Int = config.getInt("bridge.nodes-per-iteration")

  /**
    * `ttMaxSize`'s counterpart for the bit engine (`BitAnalysis`): the bit engine measurably
    * uses far less memory per node than the object-graph engine (see `doc/DoubleDummyDesign.md`'s
    * "Empirical Performance Finding"), so it can afford a bigger table than `ttMaxSize` above
    * without the same OOM risk. See `application.conf`'s `bridge.bitboard` block for exactly how
    * this value (and `bitboardNodesPerIteration` below) were chosen empirically.
    */
  val bitboardTtMaxSize: Int = config.getInt("bridge.bitboard.transposition-table.max-size")

  /**
    * `nodesPerIteration`'s counterpart for the bit engine. Deliberately NOT just "bigger for the
    * sake of it": raising this alone, with `bitboardTtMaxSize` left at `ttMaxSize`'s size, was
    * measured to do almost nothing -- once the table fills, it stops caching, and the search
    * thrashes without transposition reuse regardless of how many more nodes it's allowed. The
    * two values were chosen together against real test cases; see `application.conf`.
    */
  val bitboardNodesPerIteration: Int = config.getInt("bridge.bitboard.nodes-per-iteration")

  /**
    * The half-width of the root aspiration window passed to `withAspirationWindow` (i.e. the
    * search uses `AlphaBetaWindow(-aspirationWindow, aspirationWindow)`). This is the one free
    * parameter here -- `heuristicScale` below is *derived* from it, deliberately not
    * independently configurable, because the two must stay coupled: the aspiration-window
    * technique is only sound if every non-terminal heuristic value is guaranteed to stay
    * strictly inside the window, and only a proven result is allowed to fall outside it.
    */
  val aspirationWindow: Double = config.getDouble("bridge.aspiration-window")

  /**
    * The per-trick scale used by `BitState.heuristic` (tricks-banked-so-far * heuristicScale),
    * derived from `aspirationWindow` rather than configured independently: the worst case,
    * `Deal.TricksPerDeal` tricks in one side's favour, must stay strictly inside the window,
    * with `safetyMargin` (< 1) left as headroom. Getting this wrong the same way twice -- one
    * knob changed without the other -- is exactly the bug `aspirationWindow`'s doc just
    * described; deriving it removes the possibility rather than just documenting it.
    */
  val heuristicScale: Double =
    val safetyMargin = 0.9
    safetyMargin * aspirationWindow / Deal.TricksPerDeal
