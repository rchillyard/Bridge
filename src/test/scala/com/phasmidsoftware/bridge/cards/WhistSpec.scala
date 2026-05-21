/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.pbn.{DealValue, PBN, PBNParser}
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.io.Source
import scala.util.Try

//noinspection ScalaStyle
class WhistSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "Whist"
  it should "" in {
    val deal0 = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val north = 0
    val whist00 = Whist(deal0, north)
    whist00.deal shouldBe deal0.quit
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
    val deal = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val target = deal.hands.head
    target.nCards shouldBe 13
    val result = target.play(CardPlay(deal, None, 0, Spades, 5))
    result.nCards shouldBe 12
  }

  it should "playAll CardPlays" in {

    val deal = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val hands = deal.hands
    hands.size shouldBe 4
    val List(priority1S, priority2S, priority3S, priority4S): List[Int] =
      hands.map(_.holdings(Spades).sequences.last.priority).toList
    val trick =
      Trick.create(0, CardPlay(deal, None, 0, Spades, priority1S), CardPlay(deal, None, 1, Spades, priority2S), CardPlay(deal, None, 2, Spades, priority3S), CardPlay(deal, None, 3, Spades, priority4S))
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
  // In WhistSpec (Hand section):

  it should "promote void suit without exception" in {
    val map: Map[Suit, Holding] = Map(
      Spades -> Holding.parseHolding("SKQ3"),
      Hearts -> Holding.parseHolding("H7654"),
      Clubs -> Holding.parseHolding("C842")
      // Note: no Diamonds — hand is void in diamonds
    )
    val target = Hand(0, map)
    // Should not throw even though Diamonds is not in holdings
    noException should be thrownBy target.promote(Diamonds, 0)
  }

  behavior of "State"
  it should "create1" in {
    val deal0 = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val whist00 = Whist(deal0, 0)
    val state0 = State(whist00)
    state0.deal.nCards shouldBe 52
    state0.isConsistent shouldBe true
    val hands = deal0.hands
    val Seq(priority1S, _, _, _): Seq[Int] = hands map (_.holdings(Spades).sequences.last.priority)
    val trick1 = Trick.create(1, CardPlay(deal0, None, 0, Spades, priority1S))
    val state1 = state0.next(trick1)
    state1.deal.nCards shouldBe 51
    state1.isConsistent shouldBe true
    state1.trick.isComplete shouldBe false
    state1.trick shouldBe trick1
    state1.deal should not be deal0
  }

  it should "create2" in {
    val deal0 = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val whist00 = Whist(deal0, 0)
    val deal00 = whist00.deal
    val state0 = State(whist00)
    val hands = deal00.hands
    val Seq(priority1S, priority2S, _, _): Seq[Int] = hands map (_.holdings(Spades).sequences.last.priority)
    val trick1 = Trick.create(1, CardPlay(deal00, None, 0, Spades, priority1S))
    val trick2 = Trick.create(1, CardPlay(deal00, None, 0, Spades, priority1S), CardPlay(deal00, None, 1, Spades, priority2S))
    val state2 = state0.next(trick1).next(trick2)
    state2.deal.nCards shouldBe 50
    //		state2.isConsistent shouldBe true
    state2.trick.isComplete shouldBe false
    state2.trick shouldBe trick2
    state2.deal should not be deal00

  }

  it should "enumeratePlays to one level" in {
    val deal0 = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val whist00 = Whist(deal0, 0)
    val state0 = State(whist00)
    val states: Seq[State] = state0.enumeratePlays
    println(states)
    states.size shouldBe 10
  }

  it should "enumeratePlays to two levels" in {
    val deal0 = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val whist00 = Whist(deal0, 0)
    val state0 = State(whist00)
    val states1: Seq[State] = state0.enumeratePlays
    states1.size shouldBe 10
    val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
    states2.size shouldBe 27
  }

  it should "enumeratePlays to three levels" in {
    val deal0 = Deal.createRandom("test", 0L)
    val whist00 = Whist(deal0, 0)
    val state0 = State(whist00)
    val states1: Seq[State] = state0.enumeratePlays
    states1.size shouldBe 10
    val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
    states2.size shouldBe 25
    val states3: Seq[State] = for (p <- states2; q <- p.enumeratePlays) yield q
    states3.size shouldBe 60
  }

  it should "enumeratePlays to four levels" in {
    val deal0 = Deal.createRandom("test", 0L)
    val whist00 = Whist(deal0, 0)
    val state0 = State(whist00)
    val states1: Seq[State] = state0.enumeratePlays
    states1.size shouldBe 10
    val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
    states2.size shouldBe 25
    val states3: Seq[State] = for (p <- states2; q <- p.enumeratePlays) yield q
    states3.size shouldBe 60
    val states4: Seq[State] = for (p <- states3; q <- p.enumeratePlays) yield q
    states4.size shouldBe 144
  }

  it should "enumeratePlays to five levels" in {
    val deal0 = Deal.createRandom("test", 0L)
    val whist00 = Whist(deal0, 0)
    val state0 = State(whist00)
    val states1: Seq[State] = state0.enumeratePlays
    states1.size shouldBe 10
    val states2: Seq[State] = for (p <- states1; q <- p.enumeratePlays) yield q
    states2.size shouldBe 25
    val states3: Seq[State] = for (p <- states2; q <- p.enumeratePlays) yield q
    states3.size shouldBe 60
    val states4: Seq[State] = for (p <- states3; q <- p.enumeratePlays) yield q
    states4.size shouldBe 144
    val states5: Seq[State] = for (p <- states4; q <- p.enumeratePlays) yield q
    states5.size shouldBe 1310
  }

  behavior of "State card count invariant"

  it should "maintain deal.nCards + cardsPlayed == 52 after each play" in :
    val deal0 = Deal.createRandom("test", 0L)
    val whist0 = Whist(deal0, 0)
    val state0 = State(whist0)
    state0.whist.deal.nCards shouldBe 52
    state0.cardsPlayed shouldBe 0

    // After 1 card played
    val state1 = state0.enumeratePlays.head
    println(s"state1: trick.index=${state1.trick.index}, trick.size=${state1.trick.size}")
    println(s"state1: deal.nCards=${state1.whist.deal.nCards}, cardsPlayed=${state1.cardsPlayed}, sum=${state1.whist.deal.nCards + state1.cardsPlayed}")
    state1.whist.deal.nCards + state1.cardsPlayed shouldBe 52

    // After 2 cards played
    val state2 = state1.enumeratePlays.head
    state2.whist.deal.nCards + state2.cardsPlayed shouldBe 52

    // After 3 cards played
    val state3 = state2.enumeratePlays.head
    state3.whist.deal.nCards + state3.cardsPlayed shouldBe 52

    // After 4 cards played (trick complete, fresh trick)
    val state4 = state3.enumeratePlays.head
    state4.whist.deal.nCards + state4.cardsPlayed shouldBe 52

    // After 5 cards played (first card of trick 2)
    val state5 = state4.enumeratePlays.head
    state5.whist.deal.nCards + state5.cardsPlayed shouldBe 52

  // CONSIDER moving this to it because it takes almost 1/4 second.
  behavior of "double dummy"
  it should "analyzeDoubleDummy2" in {
    pending // Issue #14
    val target = Deal.createRandom("test", 2L)
    val whist = Whist(target, 3)
    whist.analyzeDoubleDummy(9, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy3" in {
    pending // Issue #14
    val target = Deal.createRandom("test", 3L)
    val whist = Whist(target, 3)
    whist.analyzeDoubleDummy(9, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy4" in {
    pending // Issue #14
    val target = Deal.createRandom("test", 4L)
    val whist = Whist(target, 3)
    whist.analyzeDoubleDummy(9, directionNS = true) shouldBe Some(true)
  }

  it should "analyzeDoubleDummy for suit" in {
    val target = Deal.createRandom("test", 2L)
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    println(s"Branching factor: ${initialState.enumeratePlays.size}")
    whist.analyzeDoubleDummy(9, directionNS = true, depth = 8) shouldBe Some(true)
  }

  it should "analyzeDoubleDummy for four-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ", "", "J", "3"), List("K3", "T", "", "6"), List("", "87", "Q", "8"), List("", "A", "9", "T9")))
    println(target)
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    println(s"Branching factor: ${initialState.enumeratePlays.size}")
    whist.analyzeDoubleDummy(3, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for five-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ", "9", "J", "3"), List("K32", "T", "", "6"), List("4", "87", "Q", "8"), List("5", "A", "9", "T9")))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for six-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ6", "9", "J", "3"), List("K32", "T", "T", "6"), List("4", "87", "Q", "87"), List("5", "AK", "9", "T9")))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for seven-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "3"), List("K32", "QT", "T", "6"), List("4", "87", "Q", "874"), List("5", "AK", "9", "T95")))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for eight-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "32"), List("K32", "QT", "T", "J6"), List("4", "87", "Q", "Q874"), List("5", "AK", "9", "KT95")))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for nine-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQJ76", "9", "J", "32"), List("K32", "QJT", "T", "J6"), List("4", "87", "Q8", "Q874"), List("5", "AK", "97", "KT95")))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for ten-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(
      List("AQJ76", "96", "J", "32"),
      List("K32", "QJT5", "T", "J6"),
      List("4", "874", "Q8", "Q874"),
      List("5", "AK3", "97", "KT95"))
    )
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for eleven-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(
      List("AQJ76", "96", "AJ", "32"),
      List("KT32", "QJT5", "T", "J6"),
      List("4", "8742", "Q8", "Q874"),
      List("95", "AK3", "97", "KT95")
    ))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for twelve-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(
      List("AQJ876", "96", "AJ", "32"),
      List("KT32", "QJT5", "KT", "J6"),
      List("4", "8742", "Q86", "Q874"),
      List("95", "AK3", "975", "KT95")
    ))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }
  it should "analyzeDoubleDummy for thirteen-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(
      List("AQJ876", "96", "AJ4", "32"),
      List("KT32", "QJT5", "KT3", "J6"),
      List("4", "8742", "Q862", "Q874"),
      List("95", "AK3", "975", "AKT95")
    ))
    val whist = Whist(target, 3, Some(Clubs))
    val initialState = State(whist)
    val tricks = target.nCards / Deal.CardsPerTrick
    whist.analyzeDoubleDummy(tricks, directionNS = true) shouldBe Some(false)
  }

  behavior of "analyzeDoubleDummy for four-card end position with varying leader and strain"

  private val fourCardDeal = Deal.fromHandStrings("test", "N",
    List(List("AQ", "", "J", "3"), List("K3", "T", "", "6"),
      List("", "87", "Q", "8"), List("", "A", "9", "T9")))

  private def analyzeFourCard(leader: Int, strain: Option[Suit], neededTricks: Int, expected: Option[Boolean]): Unit =
    val whist = Whist(fourCardDeal, leader, strain)
    whist.analyzeDoubleDummy(neededTricks, directionNS = true) shouldBe expected
  //
  //  // Clubs trump (original strain)
  //  it should "not make 3 tricks with clubs trump, North leads" in
  //    analyzeFourCard(0, Some(Clubs), 3, Some(false))
  //
  //  it should "make 3 tricks with clubs trump, East leads" in
  //    analyzeFourCard(1, Some(Clubs), 3, Some(false))
  //
  //  it should "make 3 tricks with clubs trump, South leads" in
  //    analyzeFourCard(2, Some(Clubs), 3, Some(true))
  //
  //  it should "make 3 tricks with clubs trump, West leads" in
  //    analyzeFourCard(3, Some(Clubs), 3, Some(true))
  //
  //  // Spades trump
  //  it should "make 3 tricks with spades trump, North leads" in
  //    analyzeFourCard(0, Some(Spades), 3, Some(false))
  //
  //  it should "make 3 tricks with spades trump, East leads" in
  //    analyzeFourCard(1, Some(Spades), 3, Some(false))
  //
  //  it should "make 3 tricks with spades trump, South leads" in
  //    analyzeFourCard(2, Some(Spades), 3, Some(true))
  //
  //  it should "make 3 tricks with spades trump, West leads" in
  //    analyzeFourCard(3, Some(Spades), 3, Some(true))
  //
  //  // Hearts trump
  //  it should "make 3 tricks with hearts trump, North leads" in
  //    analyzeFourCard(0, Some(Hearts), 3, Some(false))
  //
  //  it should "make 3 tricks with hearts trump, East leads" in
  //    analyzeFourCard(1, Some(Hearts), 3, Some(false))
  //
  //  it should "make 3 tricks with hearts trump, South leads" in
  //    analyzeFourCard(2, Some(Hearts), 3, Some(true))
  //
  //  it should "make 3 tricks with hearts trump, West leads" in
  //    analyzeFourCard(3, Some(Hearts), 3, Some(true))
  //
  //  // Notrump
  //  it should "make 3 tricks in notrump, North leads" in
  //    analyzeFourCard(0, None, 3, Some(false))
  //
  //  it should "make 3 tricks in notrump, East leads" in
  //    analyzeFourCard(1, None, 3, Some(false))
  //
  //  it should "make 3 tricks in notrump, South leads" in
  //    analyzeFourCard(2, None, 3, Some(true))
  //
  //  it should "make 3 tricks in notrump, West leads" in
  //    analyzeFourCard(3, None, 3, Some(true))

  behavior of "analyzeDoubleDummy for three-card squeeze position with varying leader and strain"

  private val threeCardAutomaticSqueezeDeal = Deal.fromHandStrings("test", "N",
    List(List("AJ", "K", "", ""), List("KQ", "A", "", ""), List("2", "2", "", "A"), List("98", "", "4", "")))

  private def analyzeThreeCardAutomaticSqueeze(leader: Int, strain: Option[Suit], neededTricks: Int, expected: Option[Boolean]): Unit =
    val whist = Whist(threeCardAutomaticSqueezeDeal, leader, strain)
    whist.analyzeDoubleDummy(neededTricks, directionNS = true) shouldBe expected

  // Clubs trump (original strain)
  it should "not make 3 tricks with clubs trump, North leads" in
    analyzeThreeCardAutomaticSqueeze(0, Some(Clubs), 3, Some(false))

  //  it should "not make 3 tricks with clubs trump, East leads" in
  //    analyzeThreeCardAutomaticSqueeze(1, Some(Clubs), 3, Some(false))

  it should "make 3 tricks with clubs trump, South leads" in
    analyzeThreeCardAutomaticSqueeze(2, Some(Clubs), 3, Some(true))

  it should "not make 3 tricks with clubs trump, West leads" in
    analyzeThreeCardAutomaticSqueeze(3, Some(Clubs), 3, Some(false))

  // Notrump
  it should "not make 3 tricks in notrump, North leads" in
    analyzeThreeCardAutomaticSqueeze(0, None, 3, Some(false))

  //  it should "not make 3 tricks in notrump, East leads" in
  //    analyzeThreeCardAutomaticSqueeze(1, None, 3, Some(false))

  it should "make 3 tricks in notrump, South leads" in
    analyzeThreeCardAutomaticSqueeze(2, None, 3, Some(true))

  //  it should "not make 3 tricks in notrump, West leads" in
  //    analyzeThreeCardAutomaticSqueeze(3, None, 3, Some(false))

  behavior of "PBN files"
  it should "analyze example5" in {
    pending // Issue #13: Takes for ever (OOM?)
    val py: Try[PBN] = PBNParser.parsePBN(Source.fromResource("com/phasmidsoftware/bridge/director/example5.pbn"))
    val deal = py.get.head("Deal").value.asInstanceOf[DealValue].deal
    val whist = Whist(deal, 1, Some(Spades))
    val z: Option[Boolean] = whist.analyzeDoubleDummy(11, true)
    z shouldBe Some(true)
  }
  it should "analyze example5 but only looking for only 10 tricks" in {
    pending // Issue #14 TRansposition Table fix
    val py: Try[PBN] = PBNParser.parsePBN(Source.fromResource("com/phasmidsoftware/bridge/director/example5.pbn"))
    val deal = py.get.head("Deal").value.asInstanceOf[DealValue].deal
    val whist = Whist(deal, 1, Some(Spades))
    val z: Option[Boolean] = whist.analyzeDoubleDummy(10, true)
    z shouldBe Some(false)
  }
  it should "analyze example5 but only looking for only 9 tricks" in {
    pending // Issue #14 TRansposition Table fix
    val py: Try[PBN] = PBNParser.parsePBN(Source.fromResource("com/phasmidsoftware/bridge/director/example5.pbn"))
    val deal = py.get.head("Deal").value.asInstanceOf[DealValue].deal
    val whist = Whist(deal, 1, Some(Spades))
    val z: Option[Boolean] = whist.analyzeDoubleDummy(9, true)
    z shouldBe Some(false)
  }
}
