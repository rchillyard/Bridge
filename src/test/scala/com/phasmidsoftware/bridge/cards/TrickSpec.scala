/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

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
}