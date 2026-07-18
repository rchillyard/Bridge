package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.bits.{BitConversions, BitState, DealBits, TrickPlay}
import com.phasmidsoftware.bridge.cards.{BridgeConfig, CacheKey, DDResult, Deal, Suit, Tricks}
import com.phasmidsoftware.gambit.game.{AlphaBetaPlayer, AlphaBetaWindow, FlatTTCache, State as GState, TTCache}

import scala.util.Random

/**
  * Entry point for the bitboard engine, mirroring `Whist.analyzeDoubleDummy`/`runPlayer`.
  *
  * Uses a transposition table keyed by `BitState.evaluateKey`, which -- having been designed
  * after the object-graph engine's `State.evaluateKey` was found to be missing trick-in-progress
  * state and the NS trick count -- includes both from the start.
  */
object BitAnalysis:

  // A trick is always 4 cards -- not a tunable assumption, so unlike nodesPerIteration/
  // aspiration-window (BridgeConfig) this isn't configurable.
  val DEPTH_STEP: Int = Deal.CardsPerTrick

  def analyzeDoubleDummy(
                           deal: DealBits,
                           openingLeader: Int,
                           strain: Option[Int],
                           neededTricks: Int,
                           directionNS: Boolean,
                           depth: Int,
                           maxNodes: Int,
                           useCanonicalKey: Boolean
                         ): DDResult =
    given gameTC: BitWhistGame = new BitWhistGame
    given stateTC: BitWhistState = new BitWhistState(neededTricks, directionNS)
    given GState[BitState, BitState] = stateTC
    given TTCache[CacheKey] = FlatTTCache(maxSize = BridgeConfig.bitboardTtMaxSize)

    val player = new AlphaBetaPlayer[BitState, BitState, TrickPlay, Int, CacheKey](
      me = if directionNS then 0 else 1,
      depth = depth
    ).withMaxNodes(maxNodes)
      .withKeyFn(s => if useCanonicalKey then s.evaluateCanonicalKey else s.evaluateKey)
      .withAspirationWindow(AlphaBetaWindow(-BridgeConfig.aspirationWindow, BridgeConfig.aspirationWindow))

    val initialState = BitState(deal, strain, openingLeader, Nil, Tricks.zero)
    runPlayer(player, directionNS, depth, initialState)

  /**
    * Convenience overload accepting a real `Deal`/`Suit`, converting at the boundary.
    *
    * @param useCanonicalKey EXPERIMENTAL, defaults to `false` (the trusted, unchanged path).
    *                        Measured (2026-07-18, after fixing `SuitMask.compact`'s per-hand
    *                        redundancy) to help substantially on a few TT-heavy stress
    *                        positions (eleven/twelve-card endings, deep Winchester board 12
    *                        searches) but to cost ~1.9x overall across the broader, more
    *                        typical real-deal IT battery (`ProblemSpec`/`AnalysisSpec`/
    *                        `WhistPBNSpec`/`WinchesterSpec`/`WinchesterBoard1Spec`/
    *                        `WinchesterBoard12Spec`: 369s -> 687s) -- most of those are
    *                        shallow searches on full 52-card deals where canonicalization's
    *                        per-node cost isn't earned back. NOT a good default as-is; see
    *                        `BitState.evaluateCanonicalKey`'s doc for the mechanism itself.
    */
  def analyzeDoubleDummy(
                           deal: Deal,
                           openingLeader: Int,
                           strain: Option[Suit],
                           neededTricks: Int,
                           directionNS: Boolean,
                           maxNodes: Int = BridgeConfig.bitboardNodesPerIteration,
                           useCanonicalKey: Boolean = false
                         ): DDResult =
    analyzeDoubleDummy(
      BitConversions.toDealBits(deal),
      openingLeader,
      BitConversions.toStrainIndex(strain),
      neededTricks,
      directionNS,
      depth = math.min(Deal.CardsPerDeal, deal.nCards),
      maxNodes = maxNodes,
      useCanonicalKey = useCanonicalKey
    )

  private def runPlayer(
                          player: AlphaBetaPlayer[BitState, BitState, TrickPlay, Int, CacheKey],
                          directionNS: Boolean,
                          depth: Int,
                          initialState: BitState
                        ): DDResult =
    player.chooseMoveIterativeDeepening(initialState, new Random(0L), DEPTH_STEP) match
      case Some((_, score, completedDepth)) =>
        val makes = if directionNS then score > 0 else score < 0
        val tricksSearched = completedDepth / Deal.CardsPerTrick
        if completedDepth >= depth then DDResult.Exact(makes, tricksSearched)
        else DDResult.Partial(makes, tricksSearched)
      case None =>
        DDResult.Inconclusive
