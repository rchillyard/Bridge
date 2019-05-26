package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

class WhistSpec extends FlatSpec with Matchers {

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

	it should "promote (4)" in {
		val target = Holding.apply(Seq(SAK, SJT), Spades)
		val promoted = target.promote(2)
		promoted.promotions.size shouldBe 1
		promoted.promotions shouldBe Seq(2)
		val quitted = promoted.quit
		quitted.size shouldBe 1
		quitted.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK"), Card("SJ"), Card("ST")))
	}

	it should "evaluate" in {
		Holding(Seq(SAK, Sequence(Seq(Card("S3"), Card("S2")))), Spades).evaluate shouldBe 2.0 +- 0.005
		Holding(Seq(SAK, SJT), Spades).evaluate shouldBe 3.0 +- 0.1
	}

	behavior of "Hand"

	it should "apply" in {
		// TODO either use the deal or the given hand map. Not both!
		val map: Map[Suit, Holding] = Map(Spades -> Holding.parseHolding("SKQ3"), Hearts -> Holding.parseHolding("H7654"), Diamonds -> Holding.parseHolding("DQ104"), Clubs -> Holding.parseHolding("C842"))
		val deal = Deal("test", 0L)
		val target = Hand(deal, 0, map)
		val holdings = target.holdings
		holdings.size shouldBe 4
		holdings.head shouldBe Spades -> Holding.parseHolding("SKQ3")
		holdings.last shouldBe Clubs -> Holding.parseHolding("C842")
	}

	it should "promote" in {
		// TODO either use the deal or the given hand map. Not both!
		val map: Map[Suit, Holding] = Map(Spades -> Holding.parseHolding("SKQ3"), Hearts -> Holding.parseHolding("H7654"), Diamonds -> Holding.parseHolding("DQ104"), Clubs -> Holding.parseHolding("C842"))
		val deal = Deal("test", 0L)
		val target = Hand(deal, 0, map)
		target.holdings.head._2.sequences.head.priority shouldBe 1
		target.holdings.last._2.sequences.head.priority shouldBe 6
		val promoted = target.promote(Spades, 0).quit
		promoted.holdings.head._2.sequences.head.priority shouldBe 0
		target.holdings.last._2.sequences.head.priority shouldBe 6
	}

	it should "play" in {
		val deal = Deal("test", 0L)
		val target = deal.hands.head
		target.cards shouldBe 13
		val result = target.play(CardPlay(deal, 0, Spades, 5))
		result.cards shouldBe 12
	}

	it should "playAll CardPlays" in {

		val deal = Deal("test", 0L)
		val hands = deal.hands
		hands.size shouldBe 4
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick = Trick.create(0, 0, Spades, CardPlay(deal, 0, Spades, priority1S), CardPlay(deal, 1, Spades, priority2S), CardPlay(deal, 2, Spades, priority3S), CardPlay(deal, 3, Spades, priority4S))
		val target0 = deal.north
		target0.cards shouldBe 13
		target0.playAll(trick).cards shouldBe 12
		val target1 = deal.east
		target1.cards shouldBe 13
		target1.playAll(trick).cards shouldBe 12
		val target2 = deal.south
		target2.cards shouldBe 13
		target2.playAll(trick).cards shouldBe 12
		val target3 = deal.south
		target3.cards shouldBe 13
		target3.playAll(trick).cards shouldBe 12
	}

	it should "form string" in {
		// TODO sort this out: we shouldn't be creating a full deal and then creating a hand.
		val deal = Deal("test", 0L)
		val target = Hand.from(deal, 0, "SAT32", "CQT98", "D43", "HKJT")
		target.neatOutput shouldBe "SAT32 HKJT D43 CQT98"
	}

	it should "evaluate" in {
		// TODO sort this out: we shouldn't be creating a full deal and then creating a hand.
		val deal = Deal("test", 0L)
		val target = Hand.from(deal, 0, "SAT32", "CQT98", "D43", "HKJT")
		target.evaluate shouldBe 2.7529296875 +- 0.02
	}

	behavior of "State"
	it should "create" in {
		val deal = Deal("test", 0L)
		val state0 = State(deal)
		state0.deal.cards shouldBe 52
		state0.isConsistent shouldBe true
		val hands = deal.hands
		val Seq(priority1S, priority2S, _, _) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick1 = Trick.create(0, 0, Spades, CardPlay(deal, 0, Spades, priority1S))
		val state1 = state0.next(trick1)
		state1.deal.cards shouldBe 51
		state1.isConsistent shouldBe true
		state1.trick.isComplete shouldBe false
		state1.trick shouldBe trick1
		state1.deal should not be deal
		val trick2 = Trick.create(0, 0, Spades, CardPlay(deal, 0, Spades, priority1S), CardPlay(deal, 1, Spades, priority2S))
		val state2 = state1.next(trick2)
		state2.deal.cards shouldBe 50
		state2.isConsistent shouldBe true
		state2.trick.isComplete shouldBe false
		state2.trick shouldBe trick2
		state2.deal should not be deal

	}

}
