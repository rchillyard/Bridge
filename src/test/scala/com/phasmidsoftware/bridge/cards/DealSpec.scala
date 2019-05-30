/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import java.io.PrintWriter

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class DealSpec extends FlatSpec with Matchers {

	behavior of "Trick"

	it should "construct" in {
		val index = 0
		val target = Trick(index, Nil)
		target.index shouldBe index
		target.plays shouldBe Nil
		target.started shouldBe false
		target.suit shouldBe None
		target.winner shouldBe None
		target.isComplete shouldBe false
		target.evaluate shouldBe 0.5
		target.isHonorLed shouldBe false
		an[CardException] should be thrownBy target.last
	}

	it should "append" in {
		val index = 0
		val nothing = Trick(index, Nil)
		val deal = Deal("test", 0L)
		val play = CardPlay(deal, 0, Spades, 0)
		val target = nothing :+ play
		target.plays shouldBe Seq(play)
		target.started shouldBe true
		target.suit shouldBe Some(Spades)
		target.winner should matchPattern { case Some(Winner(p, false)) => }
		target.isComplete shouldBe false
		target.evaluate shouldBe 0.5
		target.isHonorLed shouldBe true
		target.last shouldBe play
	}

	behavior of "Deal"

	it should "applyStringSeed0" in {
		val target = Deal("Test non-random", 0L)
		target.north.holdings(Spades) shouldBe Holding(Spades, Nine, Five)
		target.east.holdings(Hearts) shouldBe Holding(Hearts, Ace, Seven)
		target.south.holdings(Diamonds) shouldBe Holding(Diamonds, King, Eight, Deuce)
		target.west.holdings(Clubs) shouldBe Holding(Clubs, Nine, Eight, Four)
	}

	it should "applyString" in {
		val target = Deal("Test random")
		val output = target.output(Output(new PrintWriter(System.out)))
		output.close()
	}

	it should "fromCards" in {
		val newDeck: Seq[Card] =
			for (s <- Seq(Spades, Hearts, Diamonds, Clubs); r <- Seq(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)) yield Card(s, r)
		val target = Deal.fromCards("test", newDeck)
		target.north.holdings(Spades) shouldBe Holding(Spades, Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)
		target.east.holdings(Hearts) shouldBe Holding(Hearts, Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)
	}

	it should "output" in {
		val target = Deal("test", 0L)
		val writer = MockWriter()
		val output = target.output(Output(writer))
		output.close()
		writer.spilled shouldBe 115
		writer.spillway shouldBe "test\nNorth:\tS95 HQ9432 D64 CT652\nEast:\tSK742 HA7 DT93 CAQJ7\nSouth:\tSAJT86 HKT8 DK82 CK3\nWest:\tSQ3 HJ65 DAQJ75 C984\n"
	}

	it should "neatOutput" in {
		val target = Deal("test", 0L)
		target.neatOutput shouldBe "Deal test (52)\nList(S95 HQ9432 D64 CT652, SK742 HA7 DT93 CAQJ7, SAJT86 HKT8 DK82 CK3, SQ3 HJ65 DAQJ75 C984)"
	}

	it should "asCard" in {
		val deal = Deal("test", 0L)
		val cardPlay = CardPlay(deal, 0, Spades, 5)
		val card = cardPlay.asCard
		card shouldBe Card(Spades, Nine)
		card.toString shouldBe "S9"
	}

	it should "asCard 2" in {
		val deal1 = Deal("test", 0L)
		val play1 = CardPlay(deal1, 0, Spades, 5)
		val card1 = play1.asCard
		card1 shouldBe Card(Spades, Nine)
		card1.toString shouldBe "S9"
		val deal2 = deal1.play(play1)
		val play2 = CardPlay(deal2, 0, Spades, 9)
		val card2 = play2.asCard
		card2 shouldBe Card(Spades, Five)
		card2.toString shouldBe "S5"
	}

	it should "asCard 3" in {
		val deal1 = Deal("test", 0L)
		val play1 = CardPlay(deal1, 0, Spades, 5)
		val card1 = play1.asCard
		card1 shouldBe Card(Spades, Nine)
		card1.toString shouldBe "S9"
		val deal2 = deal1.play(play1)
		val play2 = CardPlay(deal2, 0, Spades, 5)
		an[CardException] should be thrownBy play2.asCard
	}

	it should "evaluate" in {
		val target = Deal("test", 0L)
		val Seq(n, _, s, _) = target.hands
		n.evaluate shouldBe 0.44 +- 0.02
		s.evaluate shouldBe 3.41 +- 0.02
		target.evaluate shouldBe (0.44 + 3.41) +- 0.03
	}

	behavior of "playAll"
	it should "playAll a trick made up of all lowest spades" in {
		val target = Deal("test", 0L)
		target.cards shouldBe 52
		val hands = target.hands
		hands.size shouldBe 4
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick =
			Trick.create(0, CardPlay(target, 0, Spades, priority1S), CardPlay(target, 1, Spades, priority2S), CardPlay(target, 2, Spades, priority3S), CardPlay(target, 3, Spades, priority4S))
		val played: Deal = target.playAll(trick)
		played.cards shouldBe 48
		val quitted = played.quit
		quitted.cards shouldBe 48
	}

	it should "playAll a trick according to strategy" in {
		val target = Deal("test", 0L)
		target.cards shouldBe 52
		val hands = target.hands
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)

		val trick =
			Trick.create(0, CardPlay(target, 0, Spades, priority1S), CardPlay(target, 1, Spades, priority2S), CardPlay(target, 2, Spades, priority3S), CardPlay(target, 3, Spades, priority4S))
		val played: Deal = target.playAll(trick)
		played.cards shouldBe 48
		val quitted = played.quit
		quitted.cards shouldBe 48
	}

	// This is part of Whist behavior
	it should "analyzeDoubleDummy2" in {
		val target = Deal("test", 2L)
		target.output(Output(new PrintWriter(System.out))).close()
		val whist = Whist(target, 3)
		whist.analyzeDoubleDummy(9, directionNS = true) shouldBe true
	}
	// This is part of Whist behavior
	ignore should "analyzeDoubleDummy0" in {
		val target = Deal("test", 0L)
		target.output(Output(new PrintWriter(System.out))).close()
		val whist = Whist(target, 0)
		whist.analyzeDoubleDummy(9, directionNS = false) shouldBe true
	}
}
