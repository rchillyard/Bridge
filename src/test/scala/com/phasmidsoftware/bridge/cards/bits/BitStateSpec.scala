/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import com.phasmidsoftware.bridge.cards.{Strain, Tricks}
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
        strain = Strain.NoTrump,
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
        strain = Strain.NoTrump,
        leader = 0,
        trickPlays = Nil,
        tricks = Tricks.zero
      )

    val low = stateWithRanks((9, 8, 7)) // hand 0 holds the top two of {7,8,9}; hand 1 holds the bottom
    val high = stateWithRanks((12, 11, 10)) // hand 0 holds the top two of {10,11,12}; hand 1 holds the bottom

    low.evaluateKey should not be high.evaluateKey // different absolute ranks: the plain key tells them apart
    low.evaluateCanonicalKey shouldBe high.evaluateCanonicalKey // same shape: the canonical key unifies them
  }

  behavior of "BitState.evaluateKey (transposition reuse across an equivalence-class ordering choice)"

  it should "reach the identical key whichever of two equivalent cards N plays first, across two tricks" in {
    // N (hand 0) holds ranks 9 and 7 of suit 0 -- adjacent live ranks with nothing (no rank 8)
    // held by ANY hand, so they're a genuine equivalence class of size 2: it should never
    // matter which one N plays first. E/S/W (hands 1/2/3) hold exactly one suit-0 card each
    // (ranks 2/3/4, all beaten by either of N's cards) plus one spare card in suit 1 -- both
    // tricks are fully forced for them (trick 1: follow suit with their only suit-0 card;
    // trick 2: discard their only remaining card, having no choice either way). Every hand
    // also keeps one untouched spare card in suit 2, so the compared end states aren't
    // trivially all-empty.
    def dealFor(nFirst: Int, nSecond: Int): DealBits = DealBits(
      HandBits.empty.setCard(0, nFirst).setCard(0, nSecond).setCard(2, 0),
      HandBits.empty.setCard(0, 2).setCard(1, 5).setCard(2, 1),
      HandBits.empty.setCard(0, 3).setCard(1, 6).setCard(2, 2),
      HandBits.empty.setCard(0, 4).setCard(1, 7).setCard(2, 3)
    )

    def playBothTricks(nFirst: Int, nSecond: Int): BitState =
      val initial = BitState(dealFor(nFirst, nSecond), strain = Strain.NoTrump, leader = 0, trickPlays = Nil, tricks = Tricks.zero)
      // Trick 1: N leads a suit-0 card; E/S/W follow suit with their only suit-0 card.
      val afterTrick1 = Seq(
        TrickPlay(0, 0, nFirst),
        TrickPlay(1, 0, 2),
        TrickPlay(2, 0, 3),
        TrickPlay(3, 0, 4)
      ).foldLeft(initial)((s, p) => s.play(p))
      afterTrick1.trickPlays shouldBe empty // trick 1 completed
      afterTrick1.leader shouldBe 0 // N's suit-0 card beats 2/3/4 either way: N wins and leads again
      afterTrick1.tricks.ns shouldBe 1 // N (NS) won trick 1
      // Trick 2: N leads its remaining (forced) suit-0 card; E/S/W are void in suit 0 and
      // discard their only remaining card (suit 1) -- forced, not a real choice.
      val afterTrick2 = Seq(
        TrickPlay(0, 0, nSecond),
        TrickPlay(1, 1, 5),
        TrickPlay(2, 1, 6),
        TrickPlay(3, 1, 7)
      ).foldLeft(afterTrick1)((s, p) => s.play(p))
      afterTrick2.trickPlays shouldBe empty // trick 2 completed
      afterTrick2.leader shouldBe 0 // N's suit-0 card is the only one following suit: N wins again
      afterTrick2.tricks.ns shouldBe 2 // N (NS) won both tricks
      afterTrick2

    val nineFirst = playBothTricks(nFirst = 9, nSecond = 7)
    val sevenFirst = playBothTricks(nFirst = 7, nSecond = 9)

    nineFirst.deal shouldBe sevenFirst.deal // same cards gone from every hand, regardless of order
    nineFirst.evaluateKey shouldBe sevenFirst.evaluateKey
    nineFirst.evaluateCanonicalKey shouldBe sevenFirst.evaluateCanonicalKey
  }

  behavior of "BitState.legalPlays (opening-lead priority scale)"

  it should "lead a singleton in a plain suit ahead of a longer suit, in a suit contract" in {
    // N holds a 3-card suit 0 (no structure), a singleton in suit 1, and a trump (suit 2) of
    // its own -- without a trump to ruff with later, leading the stiff wouldn't set up
    // anything. The singleton should be led first, regardless of the longer suit's length --
    // this is the Stiff rule the object-graph engine had and the bit engine never ported.
    val n = HandBits.empty.setCard(0, 10).setCard(0, 8).setCard(0, 6).setCard(1, 5).setCard(2, 3)
    val state = BitState(
      deal = DealBits(n, HandBits.empty, HandBits.empty, HandBits.empty),
      strain = Strain(2), leader = 0, trickPlays = Nil, tricks = Tricks.zero
    )
    state.legalPlays.head shouldBe TrickPlay(0, 1, 5)
  }

  it should "NOT prioritize the singleton if this hand itself holds no trump" in {
    // Same shape as above, but N holds no trump (suit 2) at all -- there's no ruff to set
    // up, so the singleton shouldn't be treated as special; it falls back to ordinary
    // suit-length scoring, which favors the longer suit 0.
    val n = HandBits.empty.setCard(0, 10).setCard(0, 8).setCard(0, 6).setCard(1, 5)
    val state = BitState(
      deal = DealBits(n, HandBits.empty, HandBits.empty, HandBits.empty),
      strain = Strain(2), leader = 0, trickPlays = Nil, tricks = Tricks.zero
    )
    state.legalPlays.head should not be TrickPlay(0, 1, 5)
  }

  it should "lead low from the pseudo-sequence side (K642 opposite partner's QJ), not the honor" in {
    // N holds K + a low pair, separated (from N's own perspective) by W holding a mid card;
    // partner (S) holds Q,J touching the K exactly (no gap). Notrump, so no trump exclusion.
    // The combined N+S run {K,Q,J} spans both hands -- a genuine pseudo-sequence -- so N's low
    // pair (below that run) should be led first, not the K itself.
    val n = HandBits.empty.setCard(0, 11).setCard(0, 2).setCard(0, 0) // K, and a low pair
    val e = HandBits.empty.setCard(0, 12) // the ace -- the "near seat" card the finesse plays through
    val s = HandBits.empty.setCard(0, 10).setCard(0, 9) // partner: Q, J -- touches N's K exactly
    val w = HandBits.empty.setCard(0, 6) // separates N's own K from N's own low pair
    val state = BitState(
      deal = DealBits(n, e, s, w),
      strain = Strain.NoTrump, leader = 0, trickPlays = Nil, tricks = Tricks.zero
    )
    state.legalPlays.head shouldBe TrickPlay(0, 0, 2) // the low pair's representative, not the K (rank 11)
  }

  it should "still treat it as a pseudo-sequence when the gap card is held by the near seat" in {
    // Same shape as above, but partner holds only J (not Q,J) -- a gap at Q. With the gap
    // held by E (the near seat, (leader+1)%4 -- the seat a finesse plays through), the
    // combined run should still bridge K...J, so N's low pair is still led first.
    val n = HandBits.empty.setCard(0, 11).setCard(0, 2).setCard(0, 0) // K, and a low pair
    val e = HandBits.empty.setCard(0, 10) // near seat holds the gap card (Q)
    val s = HandBits.empty.setCard(0, 9) // partner: J only
    val w = HandBits.empty.setCard(0, 6) // separates N's own K from N's own low pair
    val state = BitState(
      deal = DealBits(n, e, s, w),
      strain = Strain.NoTrump, leader = 0, trickPlays = Nil, tricks = Tricks.zero
    )
    state.legalPlays.head shouldBe TrickPlay(0, 0, 2)
  }

  it should "NOT treat it as a pseudo-sequence when the gap card is held by the far seat" in {
    // Same as above, but the gap card (Q) is held by W (the far seat, (leader+3)%4 -- too
    // late for a finesse to play through it) instead of E. The run no longer bridges, so
    // there's no pseudo-sequence bonus for N's low pair -- it falls back to ordinary scoring.
    val n = HandBits.empty.setCard(0, 11).setCard(0, 2).setCard(0, 0) // K, and a low pair
    val s = HandBits.empty.setCard(0, 9) // partner: J only
    val w = HandBits.empty.setCard(0, 10).setCard(0, 6) // far seat holds the gap card (Q) AND the separator
    val state = BitState(
      deal = DealBits(n, HandBits.empty, s, w),
      strain = Strain.NoTrump, leader = 0, trickPlays = Nil, tricks = Tricks.zero
    )
    state.legalPlays.head should not be TrickPlay(0, 0, 2)
  }

  it should "cash the honor first (not lead low) when I'm the shorter hand: Kx opposite partner's AQxx" in {
    // N holds just K + a low card (2 cards); partner (S) holds A, Q, and 2 low cards (4
    // cards) -- N is the SHORTER hand here. Even though N's own share of the combined A-K-Q
    // run is a lone honor (matching the shape that would normally favor leading low, per the
    // K642/QJ53 test above), N is shorter than partner, so N should cash the K first to avoid
    // stranding it -- not lead the low card toward partner's tenace.
    val n = HandBits.empty.setCard(0, 11).setCard(0, 1) // K, and one low card
    val s = HandBits.empty.setCard(0, 12).setCard(0, 10).setCard(0, 3).setCard(0, 5) // A, Q, and 2 low cards
    val w = HandBits.empty.setCard(0, 6) // separates N's own K from N's own low card
    val state = BitState(
      deal = DealBits(n, HandBits.empty, s, w),
      strain = Strain.NoTrump, leader = 0, trickPlays = Nil, tricks = Tricks.zero
    )
    state.legalPlays.head shouldBe TrickPlay(0, 0, 11) // the K, not the low card
  }

  it should "lead trumps when the opponents' trump lengths are unequal and the short side can ruff" in {
    // Trump is suit 3. E (hand 1) holds 3 trumps and 3 cards in plain suit 0 (the "declarer"
    // side, i.e. the longer-trump opponent, with length/losers in suit 0); W (hand 3) holds
    // only 1 trump and is void in suit 0 (the "dummy" side, short in both -- ruffing
    // potential). N should lead its only trump ahead of its unremarkable suit-1 holding.
    val n = HandBits.empty.setCard(3, 9).setCard(1, 4).setCard(1, 2)
    val e = HandBits.empty.setCard(3, 1).setCard(3, 3).setCard(3, 5).setCard(0, 2).setCard(0, 4).setCard(0, 6)
    val w = HandBits.empty.setCard(3, 7)
    val state = BitState(
      deal = DealBits(n, e, HandBits.empty, w),
      strain = Strain(3), leader = 0, trickPlays = Nil, tricks = Tricks.zero
    )
    state.legalPlays.head shouldBe TrickPlay(0, 3, 9)
  }
}
