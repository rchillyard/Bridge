package com.phasmidsoftware.bridge.cards

import java.io.PrintWriter

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class DealSpec extends FlatSpec with Matchers {

	behavior of "Deal"

	it should "applyStringSeed0" in {
		val target = Deal("Test non-random", 0L)
		target.north.holdings(Spades) shouldBe Holding(Spades, Nine, Five)
		target.east.holdings(Hearts) shouldBe Holding(Hearts, Ace, Seven)
		target.south.holdings(Diamonds) shouldBe Holding(Diamonds, King, Eight, Deuce)
		target.west.holdings(Clubs) shouldBe Holding(Clubs, Nine, Eight, Four)
	}

	it should "applyStringSeed" in {
		val target = Deal("Test random")
		val output = target.output(Output(new PrintWriter(System.out)))
		output.close()
	}

	it should "fromCards" in {
		val newDeck: Seq[Card] = for (s <- Seq(Spades, Hearts, Diamonds, Clubs); r <- Seq(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)) yield Card(s, r)
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

	it should "asCard" in {
		val deal = Deal("test", 0L)
		val cardPlay = CardPlay(deal, 0, Spades, 5)
		val card = cardPlay.asCard(deal)
		card shouldBe Card(Spades, Nine)
		card.toString shouldBe "S9"
	}

	behavior of "play"
	it should "play a trick made up of all lowest spades" in {
		val target = Deal("test", 0L)
		target.cards shouldBe 52
		val hands = target.hands
		hands.size shouldBe 4
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick = Trick.create(0, 0, Spades, CardPlay(target, 0, Spades, priority1S), CardPlay(target, 1, Spades, priority2S), CardPlay(target, 2, Spades, priority3S), CardPlay(target, 3, Spades, priority4S))
		val played: Deal = target.play(trick)
		played.cards shouldBe 48
		val quitted = played.quit
		quitted.cards shouldBe 48
	}

	it should "play a trick according to strategy" in {
		val target = Deal("test", 0L)
		target.cards shouldBe 52
		val hands = target.hands
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)

		val trick = Trick.create(0, 0, Spades, CardPlay(target, 0, Spades, priority1S), CardPlay(target, 1, Spades, priority2S), CardPlay(target, 2, Spades, priority3S), CardPlay(target, 3, Spades, priority4S))
		val played: Deal = target.play(trick)
		played.cards shouldBe 48
		val quitted = played.quit
		quitted.cards shouldBe 48
	}
}
