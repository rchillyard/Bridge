/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import org.scalatest.flatspec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class DealBitsSpec extends flatspec.AnyFlatSpec with should.Matchers {

  // Hand indices: 0=N, 1=E, 2=S, 3=W. Partners: (0,2) and (1,3). Suit index 0 used throughout.

  private def handWithRanks(ranks: Int*): HandBits =
    ranks.foldLeft(HandBits.empty)((h, r) => h.setCard(0, r))

  behavior of "DealBits.play"

  it should "clear exactly one card from exactly one hand, leaving the others untouched" in {
    val deal = DealBits(IndexedSeq(
      handWithRanks(12, 8),
      handWithRanks(11),
      handWithRanks(10),
      handWithRanks(9)
    ))
    val after = deal.play(0, 0, 12)
    after.hand(0).suitMask(0) shouldBe SuitMask.rank(8)
    after.hand(1) shouldBe deal.hand(1)
    after.hand(2) shouldBe deal.hand(2)
    after.hand(3) shouldBe deal.hand(3)
  }

  behavior of "DealBits.sideMask / opponentMask"

  it should "combine a hand and its partner for sideMask, and the other pair for opponentMask" in {
    val deal = DealBits(IndexedSeq(
      handWithRanks(12), // N
      handWithRanks(11), // E
      handWithRanks(9), // S (N's partner)
      handWithRanks(8) // W (E's partner)
    ))
    deal.sideMask(0, 0) shouldBe SuitMask.rank(12).union(SuitMask.rank(9))
    deal.opponentMask(0, 0) shouldBe SuitMask.rank(11).union(SuitMask.rank(8))
    deal.sideMask(1, 0) shouldBe SuitMask.rank(11).union(SuitMask.rank(8))
    deal.opponentMask(1, 0) shouldBe SuitMask.rank(12).union(SuitMask.rank(9))
  }

  behavior of "DealBits.equivalenceClasses"

  it should "not split a hand's cards separated only by partner's card" in {
    val deal = DealBits(IndexedSeq(
      handWithRanks(12, 10), // N holds A, Q
      handWithRanks(), // E void
      handWithRanks(11), // S (N's partner) holds K -- sits between A and Q
      handWithRanks() // W void
    ))
    deal.equivalenceClasses(0, 0) shouldBe List(SuitMask.rank(12).union(SuitMask.rank(10)))
  }

  it should "split a hand's cards separated by an opponent's card" in {
    val deal = DealBits(IndexedSeq(
      handWithRanks(12, 10), // N holds A, Q
      handWithRanks(11), // E (opponent) holds K -- sits between A and Q
      handWithRanks(),
      handWithRanks()
    ))
    deal.equivalenceClasses(0, 0).toSet shouldBe Set(SuitMask.rank(12), SuitMask.rank(10))
  }

  behavior of "DealBits.partner"

  it should "map each hand to its partner under the handIndex % 2 convention" in {
    DealBits.partner(0) shouldBe 2
    DealBits.partner(2) shouldBe 0
    DealBits.partner(1) shouldBe 3
    DealBits.partner(3) shouldBe 1
  }
}
