package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.bits.{BitConversions, BitState, DealBits, TrickPlay}
import com.phasmidsoftware.bridge.cards.{CacheKey, DDResult, Deal, Suit, Tricks}
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

  val NODES_PER_ITERATION: Int = 1_000_000
  val DEPTH_STEP: Int = 4

  def analyzeDoubleDummy(
                           deal: DealBits,
                           openingLeader: Int,
                           strain: Option[Int],
                           neededTricks: Int,
                           directionNS: Boolean,
                           depth: Int = Deal.CardsPerDeal
                         ): DDResult =
    given gameTC: BitWhistGame = new BitWhistGame
    given stateTC: BitWhistState = new BitWhistState(neededTricks, directionNS)
    given GState[BitState, BitState] = stateTC
    given TTCache[CacheKey] = FlatTTCache()

    val player = new AlphaBetaPlayer[BitState, BitState, TrickPlay, Int, CacheKey](
      me = if directionNS then 0 else 1,
      depth = depth
    ).withMaxNodes(NODES_PER_ITERATION)
      .withKeyFn(s => s.evaluateKey)
      .withAspirationWindow(AlphaBetaWindow(-0.5, 0.5))

    val initialState = BitState(deal, strain, openingLeader, Nil, Tricks.zero)
    runPlayer(player, directionNS, depth, initialState)

  /** Convenience overload accepting a real `Deal`/`Suit`, converting at the boundary. */
  def analyzeDoubleDummy(
                           deal: Deal,
                           openingLeader: Int,
                           strain: Option[Suit],
                           neededTricks: Int,
                           directionNS: Boolean
                         ): DDResult =
    analyzeDoubleDummy(
      BitConversions.toDealBits(deal),
      openingLeader,
      BitConversions.toStrainIndex(strain),
      neededTricks,
      directionNS,
      depth = math.min(Deal.CardsPerDeal, deal.nCards)
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
