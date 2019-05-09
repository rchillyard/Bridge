package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

class CardSpec extends FlatSpec with Matchers {

	behavior of "rank"

	it should "use implicit conversion" in {
		val rank2: Rank = "2"
		rank2 shouldBe Deuce
		val rankA: Rank = "A"
		rankA shouldBe Ace
	}

	it should "parse string" in {
		Rank("2") shouldBe Deuce
		Rank("T") shouldBe Ten
		Rank("10") shouldBe Ten
	}

	it should "not parse string" in {
		an[CardException] should be thrownBy Rank("Z")
	}

	it should "form string" in {
		Deuce.toString shouldBe "2"
		Ace.toString shouldBe "A"
	}

	it should "create spot rank from Int" in {
		Rank(2) shouldBe Deuce
		Rank(10) shouldBe Ten
		Rank(14) shouldBe Ace
	}

	it should "implement compare" in {
		implicitly[Ordered[Rank]](Deuce).compare(Trey) shouldBe -1
	}

	it should "sort in order" in {
		val target: List[Rank] = List(Deuce, Ace, Ten)
		target.sorted.reverse shouldBe List(Ace, Ten, Deuce)
	}

	it should "implement isHonor" in {
		Deuce.isHonor shouldBe false
		Ace.isHonor shouldBe true
	}

	it should "implement spot" in {
		Spot(9) shouldBe Nine
		Spot(10) shouldBe Ten
		Spot(11) shouldBe Jack
	}

	behavior of "suit"

	it should "use implicit conversion" in {
		val suit: Suit = "S"
		suit shouldBe Spades
	}

	it should "form string" in {
		Spades.toString shouldBe "S"
		Clubs.toString shouldBe "C"
	}

	it should "implement compare" in {
		implicitly[Ordered[Suit]](Clubs).compare(Spades) shouldBe -3
	}

	it should "parse string" in {
		Suit('S') shouldBe Spades
	}

	it should "not parse string" in {
		an[CardException] should be thrownBy Suit('X')
	}

	it should "sort in order" in {
		val target: List[Suit] = List(Diamonds, Hearts, Spades, Clubs)
		target.sorted.reverse shouldBe List(Spades, Hearts, Diamonds, Clubs)
	}

	it should "implement priority" in {
		Clubs.priority shouldBe 3
		Spades.priority shouldBe 0
		Hearts.priority shouldBe 1
		Diamonds.priority shouldBe 2
	}

	it should "implement isRound" in {
		Clubs.isRound shouldBe true
		Spades.isRound shouldBe false
		Hearts.isRound shouldBe true
		Diamonds.isRound shouldBe false
	}

	it should "implement isRed" in {
		Clubs.isRed shouldBe false
		Spades.isRed shouldBe false
		Hearts.isRed shouldBe true
		Diamonds.isRed shouldBe true
	}

	behavior of "card"

	it should "apply" in {
		val ace = Card(Spades, Ace)
		ace.rank shouldBe Ace
		ace.suit shouldBe Spades
		ace.priority shouldBe 0
		val deuce = Card(Hearts, Deuce)
		deuce.rank shouldBe Deuce
		deuce.suit shouldBe Hearts
		deuce.priority shouldBe 12
	}

	it should "use implicit conversion" in {
		val card: Card = "S2"
		card shouldBe Card(Spades, Rank("2"))
	}

	it should "form string" in {
		Card("SQ").toString shouldBe "SQ"
	}

	it should "parse string" in {
		Card("S2") shouldBe Card(Spades, Rank("2"))
	}

	it should "not parse string" in {
		an[CardException] should be thrownBy Card("X0")
	}

	it should "implement compare" in {
		implicitly[Ordered[Card]](Card(Spades, "A")).compare(Card(Spades, "K")) shouldBe 1
	}

	it should "sort in proper order" in {
		val target: List[Card] = List("SA", "DT", "S2", "SK")
		target.sorted.reverse shouldBe List[Card]("SA", "SK", "S2", "DT")
	}

	behavior of "Sequence"

	it should "apply(Int,Seq[Card])" in {
		val target = Sequence(0, Seq(Card("SA"), Card("SK")))
		target.priority shouldBe 0
		target.length shouldBe 2
	}

	it should "apply(Seq[Card])" in {
		val target = Sequence(Seq(Card("SA"), Card("SK")))
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

	behavior of "Holding"

	it should "apply" in {
		val target = Holding.apply(Seq(Sequence(Seq(Card("SA"), Card("SK"))), Sequence(Seq(Card("S3"), Card("S2")))), Spades)
		target.sequences.size shouldBe 2
		target.suit shouldBe Spades
	}

	it should "create" in {
		val holding = Holding.create(Seq(Ace, King, Ten, Nine, Seven, Five, Four), Spades)
		holding.sequences.size shouldBe 4
		holding.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK")))
	}

	it should "form string" in {
		Holding(Spades, "2", "A").toString shouldBe "{S: A[0], 2[12]}"
		Holding(Spades).toString shouldBe "{S: }"
		import Rank._
		Holding.create(Seq[Rank]("2", "A"), Spades).toString shouldBe "{S: A[0], 2[12]}"
	}

	it should "promote (1)" in {
		val target = Holding.apply(Seq(Sequence(Seq(Card("SA"), Card("SK"))), Sequence(Seq(Card("S3"), Card("S2")))), Spades)
		val promoted = target.promote(5)
		promoted.size shouldBe 2
		promoted.cards.size shouldBe 4
		promoted.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK")))
		promoted.sequences.last shouldBe Sequence(10, Seq(Card("S3"), Card("S2")))
	}

	it should "promote (2)" in {
		val target = Holding.apply(Seq(Sequence(Seq(Card("SA"), Card("SK"))), Sequence(Seq(Card("SJ"), Card("ST")))), Spades)
		val promoted = target.promote(2)
		promoted.size shouldBe 1
		promoted.sequences.head shouldBe Sequence(0, Seq(Card("SA"), Card("SK"), Card("SJ"), Card("ST")))
	}

	behavior of "Hand"

	it should "apply" in {
		val map: Map[Suit, Holding] = Map(Spades -> Holding.parseHolding("SKQ3"), Hearts -> Holding.parseHolding("H7654"), Diamonds -> Holding.parseHolding("DQ104"), Clubs -> Holding.parseHolding("C842"))
		val target = Hand(0, map)
		val holdings = target.holdings
		holdings.size shouldBe 4
		holdings.head shouldBe Spades -> Holding.parseHolding("SKQ3")
		holdings.last shouldBe Clubs -> Holding.parseHolding("C842")
	}

	it should "promote" in {
		val map: Map[Suit, Holding] = Map(Spades -> Holding.parseHolding("SKQ3"), Hearts -> Holding.parseHolding("H7654"), Diamonds -> Holding.parseHolding("DQ104"), Clubs -> Holding.parseHolding("C842"))
		val target = Hand(0, map)
		target.holdings.head._2.sequences.head.priority shouldBe 1
		target.holdings.last._2.sequences.head.priority shouldBe 6
		val promoted = target.promote(Spades, 0)
		promoted.holdings.head._2.sequences.head.priority shouldBe 0
		target.holdings.last._2.sequences.head.priority shouldBe 6
	}

	it should "play" in {
		val target = Hand(0, Map[Suit, Holding](Spades -> Holding.parseHolding("SKQ3"), Hearts -> Holding.parseHolding("H7654"), Diamonds -> Holding.parseHolding("DQ104"), Clubs -> Holding.parseHolding("C842")))
		target.cards shouldBe 13
		val index = 0
		val result = target.play(CardPlay(index, Spades, 11))
		result.cards shouldBe 12
	}

	it should "play CardPlays" in {

		val deal = Deal("test", 0L)
		val hands = deal.hands
		hands.size shouldBe 4
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick = Seq(CardPlay(0, Spades, priority1S), CardPlay(1, Spades, priority2S), CardPlay(2, Spades, priority3S), CardPlay(3, Spades, priority4S))
		val target0 = deal.north
		target0.cards shouldBe 13
		target0.play(trick).cards shouldBe 12
		val target1 = deal.east
		target1.cards shouldBe 13
		target1.play(trick).cards shouldBe 12
		val target2 = deal.south
		target2.cards shouldBe 13
		target2.play(trick).cards shouldBe 12
		val target3 = deal.south
		target3.cards shouldBe 13
		target3.play(trick).cards shouldBe 12
	}

	it should "form string" in {
		val target = Hand.from(0, "SAT32", "CQT98", "D43", "HKJT")
		target.neatOutput shouldBe "SAT32 HKJT D43 CQT98"
	}

}
