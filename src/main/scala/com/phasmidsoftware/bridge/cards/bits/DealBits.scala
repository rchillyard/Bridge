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
  *
  * Four named fields, not `IndexedSeq[HandBits]`: `HandBits` is `opaque type HandBits = Long`,
  * and a generic collection boxes it regardless -- the same disease as `SuitMask`/`List`
  * before it (see `SuitMask.equivalenceClasses`), just for this type. `play` is the hottest
  * site for it: called on every single move anywhere in the search, `hands.updated(...)`
  * boxed a `Long` every time. Named fields -- the same choice already made for `CacheKey`
  * over a generic tuple -- avoid it entirely, for a type whose size is fixed at exactly
  * four and never varies.
  */
case class DealBits(hand0: HandBits, hand1: HandBits, hand2: HandBits, hand3: HandBits):

  def hand(handIndex: Int): HandBits = handIndex match
    case 0 => hand0
    case 1 => hand1
    case 2 => hand2
    case _ => hand3

  private def withHand(handIndex: Int, h: HandBits): DealBits = handIndex match
    case 0 => copy(hand0 = h)
    case 1 => copy(hand1 = h)
    case 2 => copy(hand2 = h)
    case _ => copy(hand3 = h)

  /** Total cards remaining across all four hands. */
  def nCards: Int =
    java.lang.Long.bitCount(hand0.bits) + java.lang.Long.bitCount(hand1.bits) +
      java.lang.Long.bitCount(hand2.bits) + java.lang.Long.bitCount(hand3.bits)

  /** Play (remove) one card from one hand. */
  def play(handIndex: Int, suitIndex: Int, r: Int): DealBits =
    withHand(handIndex, hand(handIndex).clearCard(suitIndex, r))

  /** Unions the live cards of whichever hands match `side` (`handIndex % 2`) in one suit --
    * a single shared computation for `sideMask`/`opponentMask` below, instead of each chaining
    * its own `filter`/`map`/`reduce` (three allocated closures, plus boxing during `Range`
    * iteration, per call -- profiled as a real hot-path allocation source, feeding
    * `equivalenceClasses`). */
  private def unionWhere(suitIndex: Int, side: Int): SuitMask =
    if side == 0 then hand0.suitMask(suitIndex).union(hand2.suitMask(suitIndex))
    else hand1.suitMask(suitIndex).union(hand3.suitMask(suitIndex))

  /** The union of both partners' live cards in one suit -- "my side," for equivalence purposes. */
  def sideMask(handIndex: Int, suitIndex: Int): SuitMask = unionWhere(suitIndex, handIndex % 2)

  /** The union of the OTHER partnership's live cards in one suit -- this is what breaks equivalence. */
  def opponentMask(handIndex: Int, suitIndex: Int): SuitMask = unionWhere(suitIndex, 1 - handIndex % 2)

  /**
    * The equivalence classes of `handIndex`'s cards in `suitIndex`, per
    * [[SuitMask.equivalenceClasses]]: partner's cards never split a class, only a
    * still-live opponent card does.
    */
  def equivalenceClasses(handIndex: Int, suitIndex: Int): Array[SuitMask] =
    SuitMask.equivalenceClasses(hand(handIndex).suitMask(suitIndex), opponentMask(handIndex, suitIndex))

  /** The union of all four hands' live cards in one suit -- every card still in play there. */
  def suitUniverse(suitIndex: Int): SuitMask =
    hand0.suitMask(suitIndex).union(hand1.suitMask(suitIndex)).union(hand2.suitMask(suitIndex)).union(hand3.suitMask(suitIndex))

  /**
    * The four hands' masks in `suitIndex`, rank-reduced (see [[SuitMask.compact]]) against
    * that suit's own universe: only the relative order of that suit's currently-live cards
    * is preserved, not their absolute rank. Used only by `BitState.evaluateCanonicalKey`,
    * and only between tricks -- see that method's doc for why.
    */
  def canonicalSuitMasks(suitIndex: Int): IndexedSeq[SuitMask] =
    val compactor = SuitMask.compactor(suitUniverse(suitIndex))
    IndexedSeq(
      compactor(hand0.suitMask(suitIndex)),
      compactor(hand1.suitMask(suitIndex)),
      compactor(hand2.suitMask(suitIndex)),
      compactor(hand3.suitMask(suitIndex))
    )

object DealBits:
  /** The partner of a hand, under the `handIndex % 2` seating convention. */
  def partner(handIndex: Int): Int = (handIndex + 2) % 4
