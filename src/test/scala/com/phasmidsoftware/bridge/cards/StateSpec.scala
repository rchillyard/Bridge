/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.MockWriter
import com.phasmidsoftware.util.Output
import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class StateSpec extends FlatSpec with Matchers {

  behavior of "StateSpec"

  private val north = 0
  private val deal = Deal("test", 0L)
  private val whist = Whist(deal, north)
  private val trick0 = Trick.empty
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
    target.toString shouldBe "State(Whist(Deal test (51),0),T1 {S5},0:0)"
  }

  it should "neatOutput" in {
    val target = State.create(whist, trick0 :+ play0, tricks0)
    target.neatOutput shouldBe
      "State: Trick History: \"T1 {S5}\" 0:0 3.8 Deal test (51) List(S9 HQ9432 D64 CT652," +
        " SK742 HA7 DT93 CAQJ7, SAJT86 HKT8 DK82 CK3, SQ3 HJ65 DAQJ75 C984)"
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
    writer.spillway shouldBe "T1 N:S5 (3.8)"
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

  it should "enumeratePlays" in {
    val target = State(whist)
    val plays: Seq[State] = target.enumeratePlays
    plays.size shouldBe 3
    plays.head.trick.plays.size shouldBe 1
    plays.head.trick.plays.head shouldBe CardPlay(deal, north, Hearts, 10)
  }

  it should "get complex neat output" in {
    val deal = Deal("test", 0L)
    val whist0 = Whist(deal, 0)
    val state0 = State(whist0)
    val states = state0.enumeratePlays
    val state1 = states.head
    val trick1 = state1.trick
    val whist1 = state1.whist
    val trick2alternatives = trick1.enumerateSubsequentPlays(whist1)
    val state2alternatives = whist1.makeStates(state1.tricks, trick2alternatives)
    val state20 = state2alternatives.head
    val state3alternatives: Seq[State] = state20.enumeratePlays
    val state30 = state3alternatives.head
    val state4alternatives = state30.enumeratePlays
    val state40: State = state4alternatives.head
    val state5alternatives = state40.enumeratePlays
    val target = state5alternatives.head
    target.neatOutput shouldBe
      """State: Trick History: "T1 {H2, H7, HT, HJ}, T2 {D5}" 0:1 4.0 Deal test (47) List(S95 HQ943 D64 CT652, SK742 HA DT93 CAQJ7, SAJT86 HK8 DK82 CK3, SQ3 H65 DAQJ7 C984)""".stripMargin
  }

}
