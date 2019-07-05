/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class TrickSpec extends FlatSpec with Matchers {

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
		val play = CardPlay(deal, 0, Spades, 5)
		val target = nothing :+ play
		target.plays shouldBe Seq(play)
		target.started shouldBe true
		target.suit shouldBe Some(Spades)
		target.winner should matchPattern { case Some(Winner(_, false)) => }
		target.isComplete shouldBe false
		target.evaluate shouldBe 0.5
		target.isHonorLed shouldBe false
		target.last shouldBe play
	}

	it should "enumerate plays 1" in {
		val deal = Deal("test", 0L)
		val whist0 = Whist(deal, 0)
		val state0 = State(whist0)
		val states = state0.enumeratePlays
		states.size shouldBe 3
		val state1 = states.head
		val trick1 = state1.trick
		trick1.size shouldBe 1
		val whist1 = state1.whist
		whist1.deal.nCards shouldBe 51
		val openingLead = trick1.led.get
		openingLead.priority shouldBe 10
		openingLead.suit shouldBe Hearts
		openingLead.hand shouldBe 0
		openingLead.asCard shouldBe Card("H2")
		val trick2alternatives = trick1.enumerateSubsequentPlays(whist1)
		trick2alternatives.size shouldBe 2
		val state2alternatives = whist1.makeStates(state1.tricks, trick2alternatives)
		val state20 = state2alternatives.head
		val whist20 = state20.whist
		val trick20 = trick2alternatives.head
		//		trick20.cardsPlayed shouldBe 2
		val secondHandPlay0 = trick20.last
		secondHandPlay0.suit shouldBe Hearts
		secondHandPlay0.priority shouldBe 7
		val trick21 = trick2alternatives.last
		trick21.cardsPlayed shouldBe 2
		val secondHandPlay1 = trick21.last
		secondHandPlay1.suit shouldBe Hearts
		secondHandPlay1.priority shouldBe 0
		val trick3alternatives: Seq[Trick] = trick20.enumerateSubsequentPlays(whist20)
		val state3alternatives: Seq[State] = state20.enumeratePlays
		state3alternatives.size shouldBe 3
		whist20.makeStates(state20.tricks, trick3alternatives) shouldBe state3alternatives
		val state30 = state3alternatives.head
		val state4alternatives = state30.enumeratePlays
		state4alternatives.size shouldBe 2
		val state40: State = state4alternatives.head
		val state5alternatives = state40.enumeratePlays
		state5alternatives.size shouldBe 4
		println(state5alternatives)
		val state50 = state5alternatives.head
		state50.whist.deal.nCards shouldBe 47
		state50.trick.index shouldBe 2
	}
}