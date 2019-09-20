/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class HoldingSpec extends FlatSpec with Matchers {

  behavior of "Sequence"

  private val SAK = Sequence(Seq(Card("SA"), Card("SK")))
  private val SJT = Sequence(Seq(Card("SJ"), Card("ST")))

  it should "apply(Int,Seq[Card])" in {
    val target = Sequence(0, List(Card("SA"), Card("SK")))
    target.priority shouldBe 0
    target.length shouldBe 2
  }

  it should "apply(Seq[Card])" in {
    val target = SAK
    target.priority shouldBe 0
    target.length shouldBe 2
  }

  it should "promote deuce" in {
    val target = Sequence(Seq(Card("H2")))
    target.priority shouldBe 12
    target.length shouldBe 1
    val promoted = target.promote
    promoted.head shouldBe Card("H2")
    promoted.priority shouldBe 11
  }

  it should "promote ace" in {
    val target = Sequence(Seq(Card("SA")))
    target.priority shouldBe 0
    target.length shouldBe 1
    an[CardException] should be thrownBy target.promote
  }

  it should "A++K" in {
    val target1 = Sequence(Seq(Card("SA")))
    val target2 = Sequence(Seq(Card("SK")))
    target1 ++ target2 shouldBe Sequence(0, List(Card("SA"), Card("SK")))
  }

  it should "A++Q" in {
    val target1 = Sequence(Seq(Card("SA")))
    val target2 = Sequence(Seq(Card("SQ")))
    an[CardException] should be thrownBy target1 ++ target2
  }

  it should "A++Q (promoted)" in {
    val target1 = Sequence(Seq(Card("SA")))
    val target2 = Sequence(Seq(Card("SQ")))
    target1 ++ target2.promote shouldBe Sequence(0, List(Card("SA"), Card("SQ")))
  }

  it should "evaluate" in {
    SAK.evaluate shouldBe 2.0 +- 0.01
    SJT.evaluate shouldBe 0.25 +- 0.01
  }

  behavior of "Holding"

  it should "apply" in {
    val target = Holding.apply(List(SAK, Sequence(List(Card("S3"), Card("S2")))), Spades)
    target.sequences.size shouldBe 2
    target.suit shouldBe Spades
  }

  it should "create" in {
    val holding = Holding.create(Seq(Ace, King, Ten, Nine, Seven, Five, Four), Spades)
    holding.sequences.size shouldBe 4
    holding.sequences.head shouldBe Sequence(0, List(Card("SA"), Card("SK")))
  }

  it should "form string" in {
    Holding(Spades, "2", "A").toString shouldBe "{S: A[0], 2[12]} (clean)"
    Holding(Spades).toString shouldBe "{S: } (clean)"
    import Rank._
    Holding.create(Seq[Rank]("2", "A"), Spades).toString shouldBe "{S: A[0], 2[12]} (clean)"
  }

  it should "promote (1)" in {
    val target = Holding(List(SAK, Sequence(Seq(Card("S3"), Card("S2")))), Spades)
    val promoted = target.promote(5).quit
    promoted.size shouldBe 2
    promoted.cards.size shouldBe 4
    promoted.sequences.head shouldBe Sequence(0, List(Card("SA"), Card("SK")))
    promoted.sequences.last shouldBe Sequence(10, List(Card("S3"), Card("S2")))
  }

  it should "promote (2)" in {
    val target = Holding(List(SAK, SJT), Spades)
    val promoted = target.promote(2).quit
    promoted.size shouldBe 1
    promoted.sequences.head shouldBe Sequence(0, List(Card("SA"), Card("SK"), Card("SJ"), Card("ST")))
  }

  it should "promote (3)" in {
    val target = Holding(List(SJT, Sequence(Seq(Card("S3"), Card("S2")))), Spades)
    val promoted = target.promote(2).quit
    promoted.size shouldBe 2
    promoted.sequences.head shouldBe Sequence(2, List(Card("SJ"), Card("ST")))
    promoted.sequences.last shouldBe Sequence(10, List(Card("S3"), Card("S2")))
  }

  it should "promote (4)" in {
    val target = Holding(List(SAK, SJT), Spades)
    val promoted = target.promote(2)
    promoted.promotions.size shouldBe 1
    promoted.promotions shouldBe Seq(2)
    val quitted = promoted.quit
    quitted.size shouldBe 1
    quitted.sequences.head shouldBe Sequence(0, List(Card("SA"), Card("SK"), Card("SJ"), Card("ST")))
  }

  it should "-" in {
    val target = Holding(List(SJT, Sequence(Seq(Card("S3"), Card("S2")))), Spades)
    val played1 = (target - 11).promote(11).quit
    played1.size shouldBe 2
    played1.sequences.head shouldBe Sequence(3, List(Card("SJ"), Card("ST")))
    played1.sequences.last shouldBe Sequence(11, List(Card("S3")))
  }


  it should "evaluate" in {
    Holding(List(SAK, Sequence(Seq(Card("S3"), Card("S2")))), Spades).evaluate shouldBe 2.0 +- 0.005
    Holding(List(SAK, SJT), Spades).evaluate shouldBe 3.0 +- 0.1
  }

  it should "apply fourth best lead strategy" in {
    val deal = Deal("test", 0L)
    val holding: Holding = deal.hands.head.longestSuit
    val suit = holding.suit
    suit shouldBe Hearts
    // Queen lead?
    Holding.applyFollowSuitStrategy(FourthBest, Rank.lowestPriority, 2) shouldBe 26
    // Nine lead?
    Holding.applyFollowSuitStrategy(FourthBest, Rank.lowestPriority, 5) shouldBe 23
    // Four lead?
    Holding.applyFollowSuitStrategy(FourthBest, Rank.lowestPriority, 10) shouldBe 18
  }

  it should "apply win it strategy" in {
    val deal = Deal("test", 0L)
    val holding: Holding = deal.hands.head.longestSuit
    val leads: Seq[CardPlay] = holding.choosePlays(deal, None, 0, FourthBest, None)
    leads.size shouldBe 3
    val lead: CardPlay = leads.head
    val trick0 = Trick(1, List(lead), None)
    val wo = trick0.winner
    wo should matchPattern { case Some(Winner(`lead`, false)) => }
    // Ace play?
    Holding.applyFollowSuitStrategy(WinIt, wo.get.play.priority, 0) shouldBe 0
    // Seven play?
    Holding.applyFollowSuitStrategy(WinIt, wo.get.play.priority, 7) shouldBe 7
  }

  //  it should "apply cover strategy" in {
  //    val deal = Deal("test", 0L)
  //    val holding: Holding = deal.hands.head.longestSuit
  //    val suit = holding.suit
  //    val leads: Seq[CardPlay] = holding.choosePlays(deal, 0, FourthBest, None)
  //    leads.size shouldBe 3
  //    val lead: CardPlay = leads.head
  //    val trick0 = Trick(1, List(lead), None)
  //    val wo = trick0.winner
  //    wo should matchPattern { case Some(Winner(`lead`, false)) => }
  //    // Ace play?
  //    holding.applyFollowSuitStrategy(CardPlay(deal, 1, suit, 0), WinIt, 0, wo.get.play.priority) shouldBe 0
  //    // Seven play?
  //    holding.applyFollowSuitStrategy(CardPlay(deal, 1, suit, 7), WinIt, 1, wo.get.play.priority) shouldBe 7
  //  }


  it should "applyFollowSuitStrategy WinIt when 3 played" in {
    val strategy: Strategy = WinIt
    Holding.applyFollowSuitStrategy(strategy, 11, 1) shouldBe 1
    Holding.applyFollowSuitStrategy(strategy, 11, 3) shouldBe 3
    Holding.applyFollowSuitStrategy(strategy, 11, 5) shouldBe 5
    Holding.applyFollowSuitStrategy(strategy, 11, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy WinIt when T played" in {
    val strategy: Strategy = WinIt
    Holding.applyFollowSuitStrategy(strategy, 4, 1) shouldBe 1
    Holding.applyFollowSuitStrategy(strategy, 4, 3) shouldBe 3
    Holding.applyFollowSuitStrategy(strategy, 4, 5) shouldBe 23
    Holding.applyFollowSuitStrategy(strategy, 4, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy WinIt when Q played" in {
    val strategy: Strategy = WinIt
    Holding.applyFollowSuitStrategy(strategy, 2, 1) shouldBe 1
    Holding.applyFollowSuitStrategy(strategy, 2, 3) shouldBe 25
    Holding.applyFollowSuitStrategy(strategy, 2, 5) shouldBe 23
    Holding.applyFollowSuitStrategy(strategy, 2, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy WinIt when A played" in {
    val strategy: Strategy = WinIt
    Holding.applyFollowSuitStrategy(strategy, 0, 1) shouldBe 27
    Holding.applyFollowSuitStrategy(strategy, 0, 3) shouldBe 25
    Holding.applyFollowSuitStrategy(strategy, 0, 5) shouldBe 23
    Holding.applyFollowSuitStrategy(strategy, 0, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy Finesse against KJ when 3 played" in {
    val strategy: Strategy = Finesse
    Holding.applyFollowSuitStrategy(strategy, 11, 0) shouldBe 11
    Holding.applyFollowSuitStrategy(strategy, 11, 2) shouldBe 9
    Holding.applyFollowSuitStrategy(strategy, 11, 4) shouldBe 7
    Holding.applyFollowSuitStrategy(strategy, 11, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy Finesse against KJ when J played" in {
    val strategy: Strategy = Finesse
    Holding.applyFollowSuitStrategy(strategy, 3, 0) shouldBe 3
    Holding.applyFollowSuitStrategy(strategy, 3, 2) shouldBe 1
    Holding.applyFollowSuitStrategy(strategy, 3, 4) shouldBe 24
    Holding.applyFollowSuitStrategy(strategy, 3, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy Finesse against KJ when K played" in {
    val strategy: Strategy = Finesse
    Holding.applyFollowSuitStrategy(strategy, 1, 0) shouldBe 1
    Holding.applyFollowSuitStrategy(strategy, 1, 2) shouldBe 26
    Holding.applyFollowSuitStrategy(strategy, 1, 4) shouldBe 24
    Holding.applyFollowSuitStrategy(strategy, 1, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy Cover when 3 played" in {
    val strategy: Strategy = Cover
    Holding.applyFollowSuitStrategy(strategy, 11, 0) shouldBe 11
    Holding.applyFollowSuitStrategy(strategy, 11, 2) shouldBe 9
    Holding.applyFollowSuitStrategy(strategy, 11, 4) shouldBe 7
    Holding.applyFollowSuitStrategy(strategy, 11, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy Cover when J played" in {
    val strategy: Strategy = Cover
    Holding.applyFollowSuitStrategy(strategy, 3, 0) shouldBe 3
    Holding.applyFollowSuitStrategy(strategy, 3, 2) shouldBe 1
    Holding.applyFollowSuitStrategy(strategy, 3, 4) shouldBe 24
    Holding.applyFollowSuitStrategy(strategy, 3, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy Cover when K played" in {
    val strategy: Strategy = Cover
    Holding.applyFollowSuitStrategy(strategy, 1, 0) shouldBe 1
    Holding.applyFollowSuitStrategy(strategy, 1, 2) shouldBe 26
    Holding.applyFollowSuitStrategy(strategy, 1, 4) shouldBe 24
    Holding.applyFollowSuitStrategy(strategy, 1, 12) shouldBe 16
  }

  it should "applyFollowSuitStrategy Duck" in {
    val strategy: Strategy = Duck
    Holding.applyFollowSuitStrategy(strategy, 11, 0) shouldBe 28
    Holding.applyFollowSuitStrategy(strategy, 11, 2) shouldBe 26
    Holding.applyFollowSuitStrategy(strategy, 11, 4) shouldBe 24
    Holding.applyFollowSuitStrategy(strategy, 11, 12) shouldBe 16
  }

  it should "choose Play" in {
    val deal = Deal("test", 0L)
    val hand1 = deal.hands(1)
    val holding: Holding = deal.hands.head.longestSuit
    holding.suit shouldBe Hearts
    val leads: Seq[CardPlay] = holding.choosePlays(deal, None, 0, FourthBest, None)
    leads.size shouldBe 3
    val lead = leads.head
    val trick0 = Trick(1, List(lead), None)
    val wo = trick0.winner
    wo should matchPattern { case Some(Winner(`lead`, false)) => }
    val holding1 = hand1.holdings(trick0.suit.get)
    val plays = holding1.choosePlays(deal, None, 1, WinIt, wo)
    plays.size shouldBe 2
    plays.head shouldBe CardPlay(deal, None, 1, Hearts, 0)
    plays.last shouldBe CardPlay(deal, None, 1, Hearts, 7)
  }

}
