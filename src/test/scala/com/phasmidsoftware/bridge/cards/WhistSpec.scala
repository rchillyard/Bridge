/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class WhistSpec extends FlatSpec with Matchers {

	behavior of "Whist"
	it should "" in {
		val deal0 = Deal("test", 0L)
		val north = 0
		val whist00 = Whist(deal0, north)
		whist00.deal shouldBe deal0
		whist00.openingLeader shouldBe north
		val state = State(whist00, Trick.empty)
		whist00.createState shouldBe state
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
		val promoted = target.promote(Spades, 0).quit
		promoted.holdings.head._2.sequences.head.priority shouldBe 0
		target.holdings.last._2.sequences.head.priority shouldBe 6
	}

	it should "play" in {
		val deal = Deal("test", 0L)
		val target = deal.hands.head
		target.nCards shouldBe 13
		val result = target.play(CardPlay(deal, 0, Spades, 5))
		result.nCards shouldBe 12
	}

	it should "playAll CardPlays" in {

		val deal = Deal("test", 0L)
		val hands = deal.hands
		hands.size shouldBe 4
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick =
			Trick.create(0, CardPlay(deal, 0, Spades, priority1S), CardPlay(deal, 1, Spades, priority2S), CardPlay(deal, 2, Spades, priority3S), CardPlay(deal, 3, Spades, priority4S))
		val target0 = deal.north
		target0.nCards shouldBe 13
		target0.playAll(trick).nCards shouldBe 12
		val target1 = deal.east
		target1.nCards shouldBe 13
		target1.playAll(trick).nCards shouldBe 12
		val target2 = deal.south
		target2.nCards shouldBe 13
		target2.playAll(trick).nCards shouldBe 12
		val target3 = deal.south
		target3.nCards shouldBe 13
		target3.playAll(trick).nCards shouldBe 12
	}

	it should "form string" in {
		val target = Hand.from(0, "SAT32", "CQT98", "D43", "HKJT")
		target.neatOutput shouldBe "SAT32 HKJT D43 CQT98"
	}

	it should "evaluate" in {
		val target = Hand.from(0, "SAT32", "CQT98", "D43", "HKJT")
		target.evaluate shouldBe 2.7529296875 +- 0.02
	}

	behavior of "State"
	it should "create" in {
		val deal0 = Deal("test", 0L)
		val whist00 = Whist(deal0, 0)
		val state0 = State(whist00)
		state0.deal.nCards shouldBe 52
		//		state0.isConsistent shouldBe true
		val hands = deal0.hands
		val Seq(priority1S, priority2S, _, _) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick1 = Trick.create(0, CardPlay(deal0, 0, Spades, priority1S))
		val state1 = state0.next(trick1)
		state1.deal.nCards shouldBe 51
		//		state1.isConsistent shouldBe true
		state1.trick.isComplete shouldBe false
		state1.trick shouldBe trick1
		state1.deal should not be deal0
		val trick2 = Trick.create(0, CardPlay(deal0, 0, Spades, priority1S), CardPlay(deal0, 1, Spades, priority2S))
		val state2 = state1.next(trick2)
		state2.deal.nCards shouldBe 50
		//		state2.isConsistent shouldBe true
		state2.trick.isComplete shouldBe false
		state2.trick shouldBe trick2
		state2.deal should not be deal0

	}

	it should "enumeratePlays to one level" in {
		val deal0 = Deal("test", 0L)
		val whist00 = Whist(deal0, 0)
		val state0 = State(whist00)
		val states: Seq[State] = state0.enumeratePlays
		states.size shouldBe 3
		//		states foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

	it should "enumeratePlays to two levels" in {
		val deal0 = Deal("test", 0L)
		val whist00 = Whist(deal0, 0)
		val state0 = State(whist00)
		val states1: Seq[State] = state0.enumeratePlays
		states1.size shouldBe 3
		val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
		states2.size shouldBe 6
		//		states2 foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

	it should "enumeratePlays to three levels" in {
		val deal0 = Deal("test", 0L)
		val whist00 = Whist(deal0, 0)
		val state0 = State(whist00)
		val states1: Seq[State] = state0.enumeratePlays
		states1.size shouldBe 3
		val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
		states2.size shouldBe 6
		val states3: Seq[State] = for (p <- states2; q <- p.enumeratePlays) yield q
		states3.size shouldBe 18
		//		states3 foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

	it should "enumeratePlays to four levels" in {
		val deal0 = Deal("test", 0L)
		val whist00 = Whist(deal0, 0)
		val state0 = State(whist00)
		val states1: Seq[State] = state0.enumeratePlays
		states1.size shouldBe 3
		val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
		states2.size shouldBe 6
		val states3: Seq[State] = for (p <- states2; q <- p.enumeratePlays) yield q
		states3.size shouldBe 18
		val states4: Seq[State] = for (p <- states3; q <- p.enumeratePlays) yield q
		states4.size shouldBe 36
		//		states4 foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

	it should "enumeratePlays to five levels" in {
		val deal0 = Deal("test", 0L)
		val whist00 = Whist(deal0, 0)
		val state0 = State(whist00)
		val states1: Seq[State] = state0.enumeratePlays
		states1.size shouldBe 3
		val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
		states2.size shouldBe 6
		val states3: Seq[State] = for (p <- states2; q <- p.enumeratePlays) yield q
		states3.size shouldBe 18
		val states4: Seq[State] = for (p <- states3; q <- p.enumeratePlays) yield q
		states4.size shouldBe 36
		val states5: Seq[State] = for (p <- states4; q <- p.enumeratePlays) yield q
		states5.size shouldBe 139
		//		states5 foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

	behavior of "double dummy"
	it should "analyzeDoubleDummy2" in {
		val target = Deal("test", 2L)
		//		target.output(Output(new PrintWriter(System.out))).close()
		val whist = Whist(target, 3)
		println(target.neatOutput)
		whist.analyzeDoubleDummy(9, directionNS = true) shouldBe Some(true)
	}
  ignore should "analyzeDoubleDummy0" in {
		val target = Deal("test", 0L)
		//		target.output(Output(new PrintWriter(System.out))).close()
		val whist = Whist(target, 0)
		// TODO CHECK THIS
		whist.analyzeDoubleDummy(9, directionNS = false) shouldBe Some(false)
	}
}
