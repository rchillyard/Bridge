/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

	behavior of "StateSpec"

	private val north = 0
	private val deal = Deal("test", 0L)
	private val whist = Whist(deal, north)
	private val trick0 = Trick(north, Nil)
	private val tricks0 = Tricks(0, 0)
	private val play0 = CardPlay(deal, north, Spades, 9)

	it should "apply" in {
		val target = State(whist, trick0, tricks0)
		target.trick shouldBe trick0
		target.whist shouldBe whist
		target.tricks shouldBe tricks0
	}

	it should "apply1" in {
		val target = State(whist, trick0)
		target.whist shouldBe whist
		target.trick shouldBe trick0
		target.tricks shouldBe tricks0
	}

	it should "apply0" in {
		val target = State(whist)
		target.whist shouldBe whist
		target.trick shouldBe trick0
		target.tricks shouldBe tricks0
	}

	it should "create" in {
		val trick = trick0 :+ play0
		val target = State.create(whist, trick, tricks0)
		target.whist shouldBe whist.play(play0)
		target.trick shouldBe trick
		target.tricks shouldBe tricks0
	}

	it should "fail to create" in {
		an[CardException] should be thrownBy State.create(whist, trick0, tricks0)
	}

	//	it should "next" in {
	//	}

	it should "deal" in {
		val target = State(whist)
		target.deal shouldBe deal
	}

	it should "trick" in {
		val target = State(whist)
		target.trick shouldBe trick0
	}

	it should "cardsPlayed" in {
		val target = State(whist)
		target.cardsPlayed shouldBe 0
	}

	it should "cardsPlayed 2" in {
		val trick = trick0 :+ play0
		val target = State.create(whist, trick, tricks0)
		target.cardsPlayed shouldBe 1
	}

	it should "fitness" in {
		val target = State(whist)
		target.fitness shouldBe 3.9 +- 0.00001
	}

	it should "toString" in {
		val target = State.create(whist, trick0 :+ play0, tricks0)
		target.toString shouldBe "State(Whist(Deal test (51)\n{S: 9[5]} (clean)\n{H: Q[2], 9[5], 432[10]} (clean)\n{D: 6[8], 4[10]} (clean)\n{C: T[4], 65[8], 2[12]} (clean)\n{S: K[1], 7[7], 4[10], 2[12]} 9\n{H: A[0], 7[7]} (clean)\n{D: T9[4], 3[11]} (clean)\n{C: A[0], QJ[2], 7[7]} (clean)\n{S: A[0], JT[3], 8[6], 6[8]} 9\n{H: K[1], T[4], 8[6]} (clean)\n{D: K[1], 8[6], 2[12]} (clean)\n{C: K[1], 3[11]} (clean)\n{S: Q[2], 3[11]} 9\n{H: J[3], 65[8]} (clean)\n{D: A[0], QJ[2], 7[7], 5[9]} (clean)\n{C: 98[5], 4[10]} (clean),0),T0 {Play: 0 S5},0:0)"
	}

	it should "neatOutput" in {
		val target = State.create(whist, trick0 :+ play0, tricks0)
		target.neatOutput shouldBe "State: T0 {Play: 0 S5} 0:0 3.8 Deal test (51)\nList(S9 HQ9432 D64 CT652, SK742 HA7 DT93 CAQJ7, SAJT86 HKT8 DK82 CK3, SQ3 HJ65 DAQJ75 C984)"
	}

	it should "isConsistent" in {
		val target = State.create(whist, trick0 :+ play0, tricks0)
		target.isConsistent shouldBe true
	}

	it should "output" in {
		val target = State.create(whist, trick0 :+ play0, tricks0)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		writer.spilled shouldBe 13
		writer.spillway shouldBe "T0 N:S5 (3.8)"
	}

	it should "enumerateFollows" in {
		val target = State.create(whist, trick0 :+ play0, tricks0)
		val follows = target.enumerateFollows
		follows.size shouldBe 4
		follows.head.cardsPlayed shouldBe 2
		follows.head.isConsistent shouldBe true
	}

	it should "enumerateFollows bis" in {
		val target = State(whist)
		an[CardException] should be thrownBy target.enumerateFollows
	}

	it should "chooseLead" in {
		val target = State(whist)
		val leads: Seq[CardPlay] = target.chooseLead(north)
		leads.size shouldBe 3
		leads.head shouldBe CardPlay(deal, north, Hearts, 2)
	}

	it should "enumeratePlays" in {
		val target = State(whist)
		val plays: Seq[State] = target.enumeratePlays
		plays.size shouldBe 3
		plays.head.trick.plays.size shouldBe 1
		plays.head.trick.plays.head shouldBe CardPlay(deal, north, Hearts, 2)
	}


	it should "enumerateLeads" in {
		val target = State(whist)
		val leads: Seq[State] = target.enumerateLeads(north, 0)
		leads.size shouldBe 3
		leads.head.trick.plays.size shouldBe 1
		leads.head.trick.plays.head shouldBe CardPlay(deal, north, Hearts, 2)
	}

}
