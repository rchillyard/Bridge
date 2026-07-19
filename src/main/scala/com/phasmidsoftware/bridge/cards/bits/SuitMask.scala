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
  def equivalenceClasses(handBits: SuitMask, opponentBits: SuitMask): Array[SuitMask] =
    var gaps = (~opponentBits.bits) & AllBits
    // Array[SuitMask] (opaque = Int), not List[SuitMask]: a generic List always boxes its
    // elements into java.lang.Integer, profiled as a real hot-path cost -- this is called
    // once per suit, per hand, per node. RanksPerSuit is a safe upper bound on class count.
    val buf = new Array[Int](RanksPerSuit)
    var n = 0
    while gaps != 0 do
      val low = gaps & -gaps
      val run = (gaps ^ (gaps + low)) & gaps
      val cls = run & handBits.bits
      if cls != 0 then
        buf(n) = cls
        n += 1
      gaps &= ~run
    if n == buf.length then buf else buf.take(n)

  /**
    * Rank reduction: remaps `mask`'s set bits (assumed a subset of `universe`) to their
    * rank-order position within `universe` -- the highest bit in `universe` maps to the
    * highest resulting bit (`universe.size - 1`), the next-highest to `universe.size - 2`,
    * and so on down to the lowest, which maps to 0.
    *
    * What matters strategically about a suit is the RELATIVE order of its currently-live
    * cards, not their absolute rank: a suit holding 9-7-5-3 with every higher and lower
    * card already gone is strategically identical to one holding A-K-Q-J with the same
    * distribution across hands. This is a pure bit-remapping recomputed fresh from
    * whichever cards are live at the point it's called -- no history or incremental
    * bookkeeping -- the same philosophy as [[equivalenceClasses]].
    *
    * Deliberately NOT safe to use while a trick is in progress: see
    * `BitState.evaluateCanonicalKey`'s doc for why (a card already played to the current
    * trick has an absolute rank that this compaction does not, and cannot, account for).
    */
  def compact(mask: SuitMask, universe: SuitMask): SuitMask = compactor(universe)(mask)

  /**
    * Precomputes the rank-order mapping for `universe` as a reusable function.
    *
    * The mapping depends only on `universe`, not on any particular hand, so when
    * compacting several hands' masks against the SAME universe (the common case:
    * see `DealBits.canonicalSuitMasks`, which does this once per suit for all 4
    * hands), callers should build the compactor once and reuse it, rather than
    * calling `compact` separately per hand and rebuilding the mapping each time.
    * The mapping itself is a plain `Array[Int]` indexed by absolute rank, not a
    * generic `Map`, to avoid boxing and hashing overhead on this hot path.
    */
  def compactor(universe: SuitMask): SuitMask => SuitMask =
    val canonicalPositionOf = new Array[Int](RanksPerSuit)
    universe.ranks.reverse.zipWithIndex.foreach((r, i) => canonicalPositionOf(r) = i)
    (mask: SuitMask) => mask.ranks.foldLeft(SuitMask.empty)((acc, r) => acc.setRank(canonicalPositionOf(r)))
