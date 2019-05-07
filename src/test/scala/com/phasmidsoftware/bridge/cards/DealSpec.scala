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
}
