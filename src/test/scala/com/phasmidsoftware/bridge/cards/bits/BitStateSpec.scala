/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import com.phasmidsoftware.bridge.cards.Tricks
import org.scalatest.flatspec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class BitStateSpec extends flatspec.AnyFlatSpec with should.Matchers {

  private def handWithRanks(suitIndex: Int, ranks: Int*): HandBits =
    ranks.foldLeft(HandBits.empty)((h, r) => h.setCard(suitIndex, r))

  behavior of "BitState.evaluateCanonicalKey"

  it should "fall back to evaluateKey (never collide) while a trick is in progress" in {
    // Regression test for a real soundness bug found in design review, NOT hypothetical:
    // hand 0 leads suit 0 at absolute rank 6. In S1 the only remaining live card in that
    // suit (held by hand 1) is at absolute rank 10 (can beat the lead); in S2 it's at rank
    // 3 (cannot). Both have exactly one live card in that suit, so a compaction based only
    // on the REMAINING cards -- ignoring the already-played lead's absolute rank -- would
    // put both in the same canonical bucket ("one live card, canonical position 0"), a
    // false merge of two positions with different true values. The fix: evaluateCanonicalKey
    // must simply equal evaluateKey whenever trickPlays is non-empty.
    val lead = TrickPlay(handIndex = 0, suitIndex = 0, rank = 6)

    def stateWithRemainingRank(rank: Int): BitState =
      BitState(
        deal = DealBits(HandBits.empty, handWithRanks(0, rank), HandBits.empty, HandBits.empty),
        strain = None,
        leader = 0,
        trickPlays = Seq(lead),
        tricks = Tricks.zero
      )

    val s1 = stateWithRemainingRank(10) // hand 1's card CAN beat the lead
    val s2 = stateWithRemainingRank(3) // hand 1's card CANNOT beat the lead

    s1.evaluateCanonicalKey shouldBe s1.evaluateKey
    s2.evaluateCanonicalKey shouldBe s2.evaluateKey
    s1.evaluateCanonicalKey should not be s2.evaluateCanonicalKey
  }

  it should "merge two shape-identical but absolutely-different positions between tricks" in {
    // Between tricks (trickPlays.isEmpty), the mid-trick problem above cannot arise: the
    // canonical key SHOULD unify two positions with the same relative shape, even though
    // the plain evaluateKey (correctly) treats them as distinct.
    def stateWithRanks(topTwoAndBottom: (Int, Int, Int)): BitState =
      val (top, second, bottom) = topTwoAndBottom
      BitState(
        deal = DealBits(handWithRanks(0, top, second), handWithRanks(0, bottom), HandBits.empty, HandBits.empty),
        strain = None,
        leader = 0,
        trickPlays = Nil,
        tricks = Tricks.zero
      )

    val low = stateWithRanks((9, 8, 7)) // hand 0 holds the top two of {7,8,9}; hand 1 holds the bottom
    val high = stateWithRanks((12, 11, 10)) // hand 0 holds the top two of {10,11,12}; hand 1 holds the bottom

    low.evaluateKey should not be high.evaluateKey // different absolute ranks: the plain key tells them apart
    low.evaluateCanonicalKey shouldBe high.evaluateCanonicalKey // same shape: the canonical key unifies them
  }
}
