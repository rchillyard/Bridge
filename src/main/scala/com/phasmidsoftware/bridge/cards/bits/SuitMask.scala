/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

/**
  * A bitmask over the 13 ranks of one suit, held by one hand (or the union of several).
  *
  * Bit position 0 is the lowest surviving rank (e.g. Deuce) and bit position 12 is the
  * highest (Ace), the opposite direction from `Rank.priority` elsewhere in this codebase
  * (where 0 is the Ace). This direction is chosen deliberately: it makes "does card X
  * beat card Y in the same suit" a plain integer comparison of bit positions, and makes
  * rank promotion free -- once a higher card's bit is cleared from every hand, the next
  * card down is simply the new highest set bit, with no separate bookkeeping required.
  */
opaque type SuitMask = Int

object SuitMask:

  /** The number of ranks in a suit. */
  val RanksPerSuit: Int = 13

  private val AllBits: Int = (1 << RanksPerSuit) - 1

  val empty: SuitMask = 0
  val full: SuitMask = AllBits

  def apply(bits: Int): SuitMask = bits & AllBits

  /** A mask containing a single rank (0..12). */
  def rank(r: Int): SuitMask = apply(1 << r)

  extension (m: SuitMask)
    def bits: Int = m
    def isEmpty: Boolean = m == 0
    def nonEmpty: Boolean = m != 0
    def contains(r: Int): Boolean = (m & (1 << r)) != 0
    def setRank(r: Int): SuitMask = apply(m | (1 << r))
    def clearRank(r: Int): SuitMask = apply(m & ~(1 << r))
    def union(other: SuitMask): SuitMask = apply(m | other)
    def intersect(other: SuitMask): SuitMask = apply(m & other)
    def diff(other: SuitMask): SuitMask = apply(m & ~other)
    def size: Int = Integer.bitCount(m)

    /** The single highest-ranking card in this mask, or `empty` if there is none. */
    def topBit: SuitMask = Integer.highestOneBit(m)

    /** The rank (0..12) of the highest-ranking card in this mask. Undefined if `isEmpty`. */
    def topRank: Int = 31 - Integer.numberOfLeadingZeros(m)

    /** All ranks present in this mask, highest first. */
    def ranks: List[Int] =
      var remaining = m
      var acc = List.empty[Int]
      while remaining != 0 do
        val hi = 31 - Integer.numberOfLeadingZeros(remaining)
        acc = hi :: acc
        remaining &= ~(1 << hi)
      acc.reverse

  /**
    * Partitions `handBits` into equivalence classes of interchangeable cards.
    *
    * Two cards held by the same hand are interchangeable right now if no card in
    * `opponentBits` -- the OTHER partnership's still-live cards in this suit, NOT
    * "any other hand" -- has a rank between them: partner's cards never break
    * equivalence, only an opponent's card can. Cards already played by anyone are
    * simply absent from `opponentBits`, so a played card's removal automatically
    * merges the ranks around it into a bigger equivalence class -- this is how
    * promotion falls out for free, with no separate bookkeeping.
    *
    * Each returned `SuitMask` is one equivalence class (a maximal run of `handBits`
    * bits inside one opponent-free interval); its `topBit` is the natural
    * representative move to search, treating the rest as an isomorphic subgame.
    */
  def equivalenceClasses(handBits: SuitMask, opponentBits: SuitMask): List[SuitMask] =
    var gaps = (~opponentBits.bits) & AllBits
    var classes = List.empty[SuitMask]
    while gaps != 0 do
      val low = gaps & -gaps
      val run = (gaps ^ (gaps + low)) & gaps
      val cls = run & handBits.bits
      if cls != 0 then classes = SuitMask(cls) :: classes
      gaps &= ~run
    classes.reverse
