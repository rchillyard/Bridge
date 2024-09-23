/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class TrickSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Trick"

  it should "construct" in {
    val index = 0
    val target = Trick(index, Nil, None)
    target.index shouldBe index
    target.plays shouldBe Nil
    target.started shouldBe false
    target.maybeSuit shouldBe None
    target.winner shouldBe None
    target.isComplete shouldBe false
    target.evaluate shouldBe 0.5
    target.isHonorLed shouldBe false
    an[CardException] should be thrownBy target.last
  }

  it should "history" in {
    val deal = Deal("test", 0L, adjustForPartnerships = false)
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
    val state50 = state5alternatives.head
    val target = state50.trick.history
//    println(target)
    target.size shouldBe 2
    target.head shouldBe state4alternatives.head.trick
  }

  it should "append" in {
    val index = 0
    val nothing = Trick(index, Nil, None)
    val deal = Deal("test", 0L, adjustForPartnerships = false)
    val play = CardPlay(deal, None, 0, Spades, 5)
    val target = nothing :+ play
    target.plays shouldBe Seq(play)
    target.started shouldBe true
    target.maybeSuit shouldBe Some(Spades)
    target.winner should matchPattern { case Some(Winner(_, false)) => }
    target.isComplete shouldBe false
    target.evaluate shouldBe 0.5
    target.isHonorLed shouldBe false
    target.last shouldBe play
  }

  it should "not append" in {
    val deal = Deal("test", 0L, adjustForPartnerships = false)
    val play1 = CardPlay(deal, None, 0, Spades, 5) // S9
    a[CardException] should be thrownBy Trick.empty :+ play1 :+ play1
  }

  it should "pick correct winner0" in {
    val index = 0
    val deal = Deal("test", 0L, adjustForPartnerships = false)
    val play1 = CardPlay(deal, None, index, Hearts, 10) // H4
    val play2 = CardPlay(deal, None, index + 1, Hearts, 0) // HA
    val play3 = CardPlay(deal, None, index + 2, Diamonds, 12) // D2
    val play4 = CardPlay(deal, None, index + 3, Hearts, 3) // HJ
    val target = Trick.empty :+ play1 :+ play2 :+ play3 :+ play4
    target.winner.get.play.asCard shouldBe Card("HA")
  }

  it should "pick correct winner1" in {
    val index = 0
    val deal = Deal("test", 0L, adjustForPartnerships = false)
    val play1 = CardPlay(deal, None, index, Spades, 5) // S9
    val play2 = CardPlay(deal, None, index + 1, Spades, 1) // SK
    val play3 = CardPlay(deal, None, index + 2, Diamonds, 12) // D2
    val play4 = CardPlay(deal, None, index + 3, Spades, 2) // SQ
    val target = Trick.empty :+ play1 :+ play2 :+ play3 :+ play4
    target.winner.get.play.asCard shouldBe Card("SK")
  }

  it should "pick correct winner2" in {
    val index = 0
    val deal = Deal("test", 0L, adjustForPartnerships = false)
    val play1 = CardPlay(deal, None, index, Spades, 5) // S9
    val play2 = CardPlay(deal, None, index + 1, Diamonds, 11) // D3
    val play3 = CardPlay(deal, None, index + 2, Spades, 0) // SA
    val play4 = CardPlay(deal, None, index + 3, Spades, 2) // SQ
    val target = Trick.empty :+ play1 :+ play2 :+ play3 :+ play4
    target.winner.get.play.asCard shouldBe Card("SA")
  }

  it should "pick correct winner3" in {
    val index = 0
    val deal = Deal("test", 0L, adjustForPartnerships = false)
    val play1 = CardPlay(deal, Some(Diamonds), index, Spades, 5) // S9
    val play2 = CardPlay(deal, Some(Diamonds), index + 1, Diamonds, 11) // D3
    val play3 = CardPlay(deal, Some(Diamonds), index + 2, Spades, 0) // SA
    val play4 = CardPlay(deal, Some(Diamonds), index + 3, Spades, 2) // SQ
    val target = Trick.empty :+ play1 :+ play2 :+ play3 :+ play4
    target.winner.get.play.asCard shouldBe Card("D3")
  }

  it should "enumerate plays 1" in {
    val deal = Deal("test", 0L)
    val whist0 = Whist(deal, 0)
    val state0 = State(whist0)
    val states = state0.enumeratePlays
    states.size shouldBe 10
    val state1 = states.head
    val trick1 = state1.trick
    trick1.size shouldBe 1
    val whist1 = state1.whist
    whist1.deal.nCards shouldBe 51
    val openingLead = trick1.led.get
    openingLead.priority shouldBe 2
    openingLead.suit shouldBe Hearts
    openingLead.hand shouldBe 0
    openingLead.asCard shouldBe Card("HQ")
    val trick2alternatives = trick1.enumerateSubsequentPlays(whist1)
    trick2alternatives.size shouldBe 2
    val state2alternatives = whist1.makeStates(state1.tricks, trick2alternatives)
    val state20 = state2alternatives.head
    val whist20 = state20.whist
    val trick20 = trick2alternatives.head
    //		trick20.cardsPlayed shouldBe 2
    val secondHandPlay0 = trick20.last
    secondHandPlay0.suit shouldBe Hearts
    secondHandPlay0.priority shouldBe 0
    val trick21 = trick2alternatives.last
    trick21.cardsPlayed shouldBe 2
    trick21.winner.get.play.asCard shouldBe Card("HQ")
    val secondHandPlay1 = trick21.last
    secondHandPlay1.suit shouldBe Hearts
    secondHandPlay1.priority shouldBe 7
    val trick3alternatives: List[Trick] = trick20.enumerateSubsequentPlays(whist20)
    val state3alternatives: Seq[State] = state20.enumeratePlays
    state3alternatives.size shouldBe 2
    whist20.makeStates(state20.tricks, trick3alternatives) shouldBe state3alternatives
    val state30 = state3alternatives.head
    val state4alternatives = state30.enumeratePlays
    state4alternatives.size shouldBe 2
    val state40: State = state4alternatives.head
    val state5alternatives = state40.enumeratePlays
    state5alternatives.size shouldBe 9
    val state50 = state5alternatives.head
    state50.whist.deal.nCards shouldBe 47
    state50.trick.index shouldBe 2
  }
  it should "enumerate plays 2" in {
    val deal = Deal.fromHandStrings("test", "N", Seq(Seq("AQ", "", "J", "3"), Seq("K3", "T", "", "6"), Seq("", "87", "J", "8"), Seq("", "A", "9", "T9")))
    val whist0 = Whist(deal, 0, Some(Clubs))
    val state0 = State(whist0)
    val states = state0.enumeratePlays
    states.size shouldBe 4
    val state1 = states.head
    val trick1 = state1.trick
    trick1.size shouldBe 1
    val whist1 = state1.whist
    whist1.deal.nCards shouldBe 15
    val openingLead = trick1.led.get
    openingLead.priority shouldBe 0
    openingLead.suit shouldBe Spades
    openingLead.hand shouldBe 0
    openingLead.asCard shouldBe Card("SA")
    val trick2alternatives = trick1.enumerateSubsequentPlays(whist1)
    trick2alternatives.size shouldBe 2
    val state2alternatives = whist1.makeStates(state1.tricks, trick2alternatives)
    val state20 = state2alternatives.head
    val whist20 = state20.whist
    val trick20 = trick2alternatives.head
    val secondHandPlay0 = trick20.last
    secondHandPlay0.suit shouldBe Spades
    secondHandPlay0.priority shouldBe 11
    val trick21 = trick2alternatives.last
    trick21.cardsPlayed shouldBe 2
    val secondHandPlay1 = trick21.last
    secondHandPlay1.suit shouldBe Spades
    secondHandPlay1.priority shouldBe 1
    val trick3alternatives: List[Trick] = trick20.enumerateSubsequentPlays(whist20)
    val state3alternatives: Seq[State] = state20.enumeratePlays
    state3alternatives.size shouldBe 3
    state3alternatives.head.trick.plays.drop(2).head.asCard shouldBe Card(Clubs, Eight)
    whist20.makeStates(state20.tricks, trick3alternatives) shouldBe state3alternatives
    val state30 = state3alternatives.head
    val state4alternatives = state30.enumeratePlays
    state4alternatives.size shouldBe 3
    val state40: State = state4alternatives.head
    val state5alternatives = state40.enumeratePlays
    state5alternatives.size shouldBe 3
    val state50 = state5alternatives.head
    state50.whist.deal.nCards shouldBe 11
    state50.trick.index shouldBe 2
  }
}