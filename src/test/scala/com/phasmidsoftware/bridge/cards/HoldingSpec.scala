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
    val target = Sequence(0, Seq(Card("SA"), Card("SK")))
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
    target1 ++ target2 shouldBe Sequence(0, Seq(Card("SA"), Card("SK")))
  }

  it should "A++Q" in {
    val target1 = Sequence(Seq(Card("SA")))
    val target2 = Sequence(Seq(Card("SQ")))
    an[CardException] should be thrownBy target1 ++ target2
  }

  it should "A++Q (promoted)" in {
    val target1 = Sequence(Seq(Card("SA")))
    val target2 = Sequence(Seq(Card("SQ")))
    target1 ++ target2.promote shouldBe Sequence(0, Seq(Card("SA"), Card("SQ")))
  }

  it should "evaluate" in {
    SAK.evaluate shouldBe 2.0 +- 0.01
    SJT.evaluate shouldBe 0.25 +- 0.01
  }

  behavior of "Holding"

  it should "apply" in {
    val target = Holding.apply(Seq(SAK, Sequence(Seq(Card("S3"), Card("S2")))), Spades)
    target.sequences.size shouldBe 2
    target.suit shouldBe Spades
  }

  it should "create" in {
    val holding = Holding.create(Seq(Ace, King, Ten, Nine, Seven, Five, Four), Spades)
    holding.sequences.size shouldBe 4
    holding.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK")))
  }

  it should "form string" in {
    Holding(Spades, "2", "A").toString shouldBe "{S: A[0], 2[12]} (clean)"
    Holding(Spades).toString shouldBe "{S: } (clean)"
    import Rank._
    Holding.create(Seq[Rank]("2", "A"), Spades).toString shouldBe "{S: A[0], 2[12]} (clean)"
  }

  it should "promote (1)" in {
    val target = Holding.apply(Seq(SAK, Sequence(Seq(Card("S3"), Card("S2")))), Spades)
    val promoted = target.promote(5).quit
    promoted.size shouldBe 2
    promoted.cards.size shouldBe 4
    promoted.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK")))
    promoted.sequences.last shouldBe Sequence(10, Seq(Card("S3"), Card("S2")))
  }

  it should "promote (2)" in {
    val target = Holding.apply(Seq(SAK, SJT), Spades)
    val promoted = target.promote(2).quit
    promoted.size shouldBe 1
    promoted.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK"), Card("SJ"), Card("ST")))
  }

  it should "promote (3)" in {
    val target = Holding.apply(Seq(SJT, Sequence(Seq(Card("S3"), Card("S2")))), Spades)
    val promoted = target.promote(2).quit
    promoted.size shouldBe 2
    promoted.sequences.head shouldBe Sequence(2, Seq(Card("SJ"), Card("ST")))
    promoted.sequences.last shouldBe Sequence(10, Seq(Card("S3"), Card("S2")))
  }

  it should "promote (4)" in {
    val target = Holding.apply(Seq(SAK, SJT), Spades)
    val promoted = target.promote(2)
    promoted.promotions.size shouldBe 1
    promoted.promotions shouldBe Seq(2)
    val quitted = promoted.quit
    quitted.size shouldBe 1
    quitted.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK"), Card("SJ"), Card("ST")))
  }

  it should "-" in {
    val target = Holding.apply(Seq(SJT, Sequence(Seq(Card("S3"), Card("S2")))), Spades)
    val played1 = (target - 11).promote(11).quit
    println(played1)
    played1.size shouldBe 2
    played1.sequences.head shouldBe Sequence(3, Seq(Card("SJ"), Card("ST")))
    played1.sequences.last shouldBe Sequence(11, Seq(Card("S3")))
  }


  it should "evaluate" in {
    Holding(Seq(SAK, Sequence(Seq(Card("S3"), Card("S2")))), Spades).evaluate shouldBe 2.0 +- 0.005
    Holding(Seq(SAK, SJT), Spades).evaluate shouldBe 3.0 +- 0.1
  }

  it should "apply fourth best lead strategy" in {
    val deal = Deal("test", 0L)
    val holding: Holding = deal.hands.head.longestSuit
    val suit = holding.suit
    suit shouldBe Hearts
    // Queen lead?
    holding.applyStrategy(CardPlay(deal, 0, suit, 2), FourthBest, 0, Rank.lowestPriority) shouldBe 12
    // Nine lead?
    holding.applyStrategy(CardPlay(deal, 0, suit, 5), FourthBest, 1, Rank.lowestPriority) shouldBe 9
    // Four lead?
    holding.applyStrategy(CardPlay(deal, 0, suit, 10), FourthBest, 2, Rank.lowestPriority) shouldBe 4
  }

  it should "apply win it strategy" in {
    val deal = Deal("test", 0L)
    val holding: Holding = deal.hands.head.longestSuit
    val suit = holding.suit
    val leads: Seq[CardPlay] = holding.choosePlays(deal, 0, FourthBest, None)
    leads.size shouldBe 3
    val lead: CardPlay = leads.head
    val trick0 = Trick(1, Seq(lead))
    val wo = trick0.winner
    wo should matchPattern { case Some(Winner(`lead`, false)) => }
    // Ace play?
    holding.applyStrategy(CardPlay(deal, 1, suit, 0), WinIt, 0, wo.get.play.priority) shouldBe 0
    // Seven play?
    holding.applyStrategy(CardPlay(deal, 1, suit, 7), WinIt, 1, wo.get.play.priority) shouldBe 7
  }

  it should "choose Play" in {
    val deal = Deal("test", 0L)
    val hand1 = deal.hands(1)
    val holding: Holding = deal.hands.head.longestSuit
    holding.suit shouldBe Hearts
    val leads: Seq[CardPlay] = holding.choosePlays(deal, 0, FourthBest, None)
    leads.size shouldBe 3
    val lead = leads.head
    val trick0 = Trick(1, Seq(lead))
    val wo = trick0.winner
    wo should matchPattern { case Some(Winner(`lead`, false)) => }
    val holding1 = hand1.holdings(trick0.suit.get)
    val plays = holding1.choosePlays(deal, 1, WinIt, wo)
    plays.size shouldBe 2
    plays.head shouldBe CardPlay(deal, 1, Hearts, 0)
    plays.last shouldBe CardPlay(deal, 1, Hearts, 7)
  }

}
