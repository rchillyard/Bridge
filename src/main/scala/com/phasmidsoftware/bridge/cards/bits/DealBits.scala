/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

/**
  * A compact bitboard representation of one hand's holding across all four suits,
  * packed into a single `Long`: bit index = `suitIndex*13 + rankBit`, mirroring the
  * packing already used by `State.evaluateKey` (`suit.priority*13 + seq.priority`),
  * except keyed on raw rank (see [[SuitMask]]) rather than collapsed sequence-priority.
  */
opaque type HandBits = Long

object HandBits:

  val empty: HandBits = 0L

  def apply(bits: Long): HandBits = bits

  private def shift(suitIndex: Int): Int = suitIndex * SuitMask.RanksPerSuit

  extension (h: HandBits)
    def bits: Long = h
    def isEmpty: Boolean = h == 0L

    def suitMask(suitIndex: Int): SuitMask =
      SuitMask(((h >>> shift(suitIndex)) & SuitMask.full.bits).toInt)

    def withSuitMask(suitIndex: Int, mask: SuitMask): HandBits =
      val cleared = h & ~(SuitMask.full.bits.toLong << shift(suitIndex))
      cleared | (mask.bits.toLong << shift(suitIndex))

    def clearCard(suitIndex: Int, r: Int): HandBits =
      h & ~(1L << (shift(suitIndex) + r))

    def setCard(suitIndex: Int, r: Int): HandBits =
      h | (1L << (shift(suitIndex) + r))

/**
  * The four hands' holdings in one deal, indexed 0..3 (N, E, S, W by the convention
  * used elsewhere in this codebase: partners are `handIndex % 2 == 0` vs `== 1`).
  */
case class DealBits(hands: IndexedSeq[HandBits]):

  def hand(handIndex: Int): HandBits = hands(handIndex)

  /** Total cards remaining across all four hands. */
  def nCards: Int = hands.map(h => java.lang.Long.bitCount(h.bits)).sum

  /** Play (remove) one card from one hand. */
  def play(handIndex: Int, suitIndex: Int, r: Int): DealBits =
    DealBits(hands.updated(handIndex, hands(handIndex).clearCard(suitIndex, r)))

  /** The union of both partners' live cards in one suit -- "my side," for equivalence purposes. */
  def sideMask(handIndex: Int, suitIndex: Int): SuitMask =
    val side = handIndex % 2
    hands.indices.filter(_ % 2 == side).map(hands(_).suitMask(suitIndex)).reduce(_.union(_))

  /** The union of the OTHER partnership's live cards in one suit -- this is what breaks equivalence. */
  def opponentMask(handIndex: Int, suitIndex: Int): SuitMask =
    val side = handIndex % 2
    hands.indices.filterNot(_ % 2 == side).map(hands(_).suitMask(suitIndex)).reduce(_.union(_))

  /**
    * The equivalence classes of `handIndex`'s cards in `suitIndex`, per
    * [[SuitMask.equivalenceClasses]]: partner's cards never split a class, only a
    * still-live opponent card does.
    */
  def equivalenceClasses(handIndex: Int, suitIndex: Int): List[SuitMask] =
    SuitMask.equivalenceClasses(hands(handIndex).suitMask(suitIndex), opponentMask(handIndex, suitIndex))

  /** The union of all four hands' live cards in one suit -- every card still in play there. */
  def suitUniverse(suitIndex: Int): SuitMask =
    hands.map(_.suitMask(suitIndex)).reduce(_.union(_))

  /**
    * The four hands' masks in `suitIndex`, rank-reduced (see [[SuitMask.compact]]) against
    * that suit's own universe: only the relative order of that suit's currently-live cards
    * is preserved, not their absolute rank. Used only by `BitState.evaluateCanonicalKey`,
    * and only between tricks -- see that method's doc for why.
    */
  def canonicalSuitMasks(suitIndex: Int): IndexedSeq[SuitMask] =
    val compactor = SuitMask.compactor(suitUniverse(suitIndex))
    hands.map(h => compactor(h.suitMask(suitIndex)))

object DealBits:
  /** The partner of a hand, under the `handIndex % 2` seating convention. */
  def partner(handIndex: Int): Int = (handIndex + 2) % 4
