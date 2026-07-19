/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

/**
  * PARKED, NOT WIRED IN -- tried 2026-07-18, reverted the same day. Captured here for
  * reference in case it's worth revisiting with a cheaper implementation, not because it
  * worked.
  *
  * A bit-native port of the object-graph engine's card-potential evaluation
  * (`Deal.evaluate`/`Holding.evaluate`), intended to replace `BitState.heuristic`'s
  * tricks-banked-so-far-only estimate with something that also accounts for each side's
  * remaining trick-taking strength -- the same idea already ported for move ordering
  * (`BitState.leadScore`/`followSuitScore`/`discardScore`), applied to the non-terminal
  * heuristic instead.
  *
  * The formula: each of a hand's equivalence classes (`DealBits.equivalenceClasses`, the
  * bit-native analogue of a `Sequence`) contributes `size * 0.5^priority`, where `priority`
  * is how many still-live cards (across all four hands) outrank its top card -- the
  * bit-native equivalent of `Sequence.priority`'s already-promotion-aware rank. Bounded by
  * a hand's own remaining card count, since `0.5^priority <= 1` and every card belongs to
  * exactly one class -- so NS's or EW's total potential is at most `Deal.CardsPerDeal / 2`
  * (26), making the combined (tricks-difference + potential-difference) heuristic safely
  * boundable for the aspiration window, the same way `BridgeConfig.heuristicScale` already
  * is for the trick-count-only version (worst case 13 + 26 = 39, requiring
  * `heuristicScale` to be re-derived against that bound instead of just `Deal.TricksPerDeal`
  * if this is ever wired back in).
  *
  * Deliberately simpler than `Holding._evaluate`: the object-graph version additionally
  * up-weights a hand's LOWER sequences in the same suit by `2^(cards in higher sequences of
  * that suit)`. That extra term isn't cleanly bounded by a hand's own card count the way
  * this version is, and the object-graph code's own comment already flags it as not "done
  * properly" -- left out rather than ported with an unverified bound.
  *
  * == Why it's parked: too slow, no measured benefit ==
  *
  * Verified correctness-neutral (380/380 on the default suite) but empirically useless and
  * expensive when it was wired into `BitState.heuristic` in place of the tricks-only
  * estimate:
  *   - Zero change to the known ten/eleven/twelve/thirteen-card `BitAnalysisITSpec` gap --
  *     identical wrong answers, identical search depths, on the eleven- and twelve-card
  *     cases this was hoped to help with.
  *   - Severe slowdown: an isolated bit-engine-only benchmark (`WinchesterBoard12Spec`'s
  *     "NEW engine" cases, normally ~11 seconds) didn't finish in 5+ minutes and had to be
  *     killed.
  *
  * Most likely cause: Gambit's `AlphaBetaPlayer.orderedMoves` calls `state.heuristic(next)`
  * once per CANDIDATE MOVE (to sort them), not once per node -- so a heuristic this much
  * more expensive than the O(1) trick-count subtraction gets multiplied by the branching
  * factor at every node, not just paid once per node. The implementation below also
  * recomputes each suit's live-card mask once per hand (four times) rather than once
  * overall, a real but likely secondary inefficiency -- fixing that alone would not have
  * closed a 5-minutes-and-still-running gap against an ~11-second baseline.
  *
  * If this is ever revisited, it would need either a fundamentally cheaper computation
  * (precompute each suit's live mask once per node and share it across hands, rather than
  * recomputing per hand) or a structural change in Gambit to decouple the cheap
  * move-ordering heuristic from a richer position-evaluation one, rather than an
  * optimization pass on what's captured here.
  */
object CardPotentialHeuristic:

  /**
    * The card-potential contribution of one hand: summed over its suits, each equivalence
    * class contributes `size * 0.5^priority`. Bounded by this hand's own remaining card
    * count, since every one of its cards belongs to exactly one class and `0.5^priority <= 1`.
    */
  def cardPotential(deal: DealBits, handIndex: Int): Double =
    (0 until 4).map { suitIndex =>
      val liveMask = (0 until 4).map(h => deal.hand(h).suitMask(suitIndex)).reduce(_.union(_))
      deal.equivalenceClasses(handIndex, suitIndex).map { cls =>
        val ranksAbove = liveMask.bits & ~((1 << (cls.topRank + 1)) - 1)
        val priority = Integer.bitCount(ranksAbove)
        cls.size * math.pow(0.5, priority)
      }.sum
    }.sum

  /** NS-minus-EW card-potential difference, for combining with a tricks-banked difference. */
  def cardPotentialDifference(deal: DealBits): Double =
    cardPotential(deal, 0) + cardPotential(deal, 2) - cardPotential(deal, 1) - cardPotential(deal, 3)
