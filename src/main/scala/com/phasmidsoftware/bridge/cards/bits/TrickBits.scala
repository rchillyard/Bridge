/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

/**
  * A single play within a trick, expressed in bitboard terms: which hand played,
  * which suit, and which rank (`SuitMask` bit-position convention: 0 = lowest
  * surviving rank, 12 = highest, e.g. Ace).
  */
case class TrickPlay(handIndex: Int, suitIndex: Int, rank: Int)

/**
  * Trick-winner determination, ported from `Trick.score` (which uses a three-tier
  * scheme: off-suit discard, followed the led suit, or ruffed/trumped) onto rank
  * bit-positions instead of `Rank.priority`.
  *
  * Trump is not a per-suit bit concept here, any more than it is on `Card`/`Suit`
  * elsewhere in this codebase -- it is only ever compared as "does this play's suit
  * equal the optional trump suit," at the point of scoring a trick.
  */
object TrickBits:

  /**
    * The gap between tiers. Ranks span exactly `SuitMask.RanksPerSuit` (13) distinct
    * values (0..12), so a gap of 13 is the minimum that guarantees no overlap between
    * tiers: the worst score in a tier (`base + 0`) exceeds the best score of the tier
    * below it (`baseBelow + 12`) iff `base - baseBelow >= 13`.
    */
  private val TierGap: Int = SuitMask.RanksPerSuit

  /**
    * The score of a single play: higher is better, and any play in a higher tier
    * (followed suit, then ruffed/trumped) always outscores any play in a lower tier,
    * regardless of rank within either tier.
    *
    * @param rank       the rank bit-position of the card played (0..12, 12 = best).
    * @param followsSuit true if this play is in the suit that was led.
    * @param isRuff      true if this play is a trump play of a non-trump led suit.
    */
  def score(rank: Int, followsSuit: Boolean, isRuff: Boolean): Int =
    val base = if followsSuit then TierGap else if isRuff then 2 * TierGap else 0
    base + rank

  /**
    * The score of a [[TrickPlay]] given the suit that was led and the trump suit (if any).
    */
  def score(play: TrickPlay, ledSuit: Int, trumpSuit: Option[Int]): Int =
    val followsSuit = play.suitIndex == ledSuit
    val isRuff = !followsSuit && trumpSuit.contains(play.suitIndex)
    score(play.rank, followsSuit, isRuff)

  /**
    * The winning play of a completed trick.
    */
  def winningPlay(plays: Seq[TrickPlay], ledSuit: Int, trumpSuit: Option[Int]): TrickPlay =
    plays.maxBy(score(_, ledSuit, trumpSuit))
