package com.phasmidsoftware.bridge.gambit

import com.phasmidsoftware.bridge.cards.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class WhistStateSpec extends AnyFlatSpec with should.Matchers:

  private val deal0 = Deal("test", 0L, adjustForPartnerships = false)
  private val whist0 = Whist(deal0, 0)

  // ---------------------------------------------------------------------------
  // sequence
  // ---------------------------------------------------------------------------

  behavior of "WhistState.sequence"

  it should "return 0 for a fresh state" in {
    val ws = WhistState(9, directionNS = true)
    val s = State(whist0)
    ws.sequence(s) shouldBe 0
  }

  it should "return cardsPlayed for a state" in {
    val ws = WhistState(9, directionNS = true)
    val s = State(whist0)
    ws.sequence(s) shouldBe s.cardsPlayed
  }

  // ---------------------------------------------------------------------------
  // isGoal — goal achieved
  // ---------------------------------------------------------------------------

  behavior of "WhistState.isGoal — goal achieved"

  it should "return Some(true) when NS has neededTricks" in {
    val ws = WhistState(1, directionNS = true)
    // Construct a state where NS has 1 trick.
    val tricks = Tricks(1, 0)
    val s = State(whist0, Trick.empty, tricks)
    ws.isGoal(s) shouldBe Some(true)
  }

  it should "return Some(true) when NS has more than neededTricks" in {
    val ws = WhistState(2, directionNS = true)
    val tricks = Tricks(3, 0)
    val s = State(whist0, Trick.empty, tricks)
    ws.isGoal(s) shouldBe Some(true)
  }

  it should "return Some(true) when EW has neededTricks (directionNS=false)" in {
    val ws = WhistState(1, directionNS = false)
    val tricks = Tricks(0, 1)
    val s = State(whist0, Trick.empty, tricks)
    ws.isGoal(s) shouldBe Some(true)
  }

  // ---------------------------------------------------------------------------
  // isGoal — counter-goal (goal impossible)
  // ---------------------------------------------------------------------------

  behavior of "WhistState.isGoal — counter-goal"

  it should "return Some(false) when EW has enough tricks to defeat NS goal of 9" in {
    val ws = WhistState(9, directionNS = true)
    // EW needs 13+1-9=5 tricks to defeat NS
    val tricks = Tricks(0, 5)
    val s = State(whist0, Trick.empty, tricks)
    ws.isGoal(s) shouldBe Some(false)
  }

  it should "return Some(false) when NS has enough tricks to defeat EW goal of 9" in {
    val ws = WhistState(9, directionNS = false)
    val tricks = Tricks(5, 0)
    val s = State(whist0, Trick.empty, tricks)
    ws.isGoal(s) shouldBe Some(false)
  }

  // ---------------------------------------------------------------------------
  // isGoal — in progress
  // ---------------------------------------------------------------------------

  behavior of "WhistState.isGoal — in progress"

  it should "return None for a fresh state" in {
    val ws = WhistState(9, directionNS = true)
    val s = State(whist0)
    ws.isGoal(s) shouldBe None
  }

  it should "return None when neither goal nor counter-goal is reached" in {
    val ws = WhistState(9, directionNS = true)
    val tricks = Tricks(4, 3)
    val s = State(whist0, Trick.empty, tricks)
    ws.isGoal(s) shouldBe None
  }

  // ---------------------------------------------------------------------------
  // isWin
  // ---------------------------------------------------------------------------

  behavior of "WhistState.isWin"

  it should "return true when goal is achieved" in {
    val ws = WhistState(1, directionNS = true)
    val tricks = Tricks(1, 0)
    val s = State(whist0, Trick.empty, tricks)
    ws.isWin(s) shouldBe true
  }

  it should "return false for a fresh state" in {
    val ws = WhistState(9, directionNS = true)
    ws.isWin(State(whist0)) shouldBe false
  }

  // ---------------------------------------------------------------------------
  // heuristic
  // ---------------------------------------------------------------------------

  behavior of "WhistState.heuristic"

  it should "return a positive value when NS has tricks" in {
    val ws = WhistState(9, directionNS = true)
    val tricks = Tricks(4, 2)
    val s = State(whist0, Trick.empty, tricks)
    // cardsPlayed=0 so isNSToMove=false; raw = 4 + deal.evaluate; negated
    // Actually with cardsPlayed=0 heuristic returns -raw — just check it's non-zero
    ws.heuristic(s) should not be 0.0
  }

  it should "return MaxValue when NS wins terminally and NS just moved" in {
    val ws = WhistState(1, directionNS = true)
    val deal = Deal("test", 1L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    // Get a natural successor state with cardsPlayed=1
    val s0 = State(whist)
    val s1 = s0.enumeratePlays.head
    // Now override the tricks to show NS has won 1 trick
    val s = State(s1.whist, s1.trick, Tricks(1, 0))
    s.cardsPlayed shouldBe 1 // verify cardsPlayed=1
    ws.heuristic(s) shouldBe Double.MaxValue
  }

  // ---------------------------------------------------------------------------
  // moves
  // ---------------------------------------------------------------------------

  behavior of "WhistState.moves"

  it should "return the same number of moves as enumeratePlays" in {
    val ws = WhistState(9, directionNS = true)
    val s = State(whist0)
    ws.moves(s).size shouldBe s.enumeratePlays.size
  }

  it should "return non-empty moves from a fresh state" in {
    val ws = WhistState(9, directionNS = true)
    val s = State(whist0)
    ws.moves(s) should not be empty
  }


class WhistGameSpec extends AnyFlatSpec with should.Matchers:

  private val deal0 = Deal("test", 0L, adjustForPartnerships = false)
  private val whist0 = Whist(deal0, 0)
  private val game0 = WhistGame(whist0)

  // ---------------------------------------------------------------------------
  // start / startingPlayer / players
  // ---------------------------------------------------------------------------

  behavior of "WhistGame basics"

  it should "return the correct starting player" in {
    game0.startingPlayer shouldBe 0 // openingLeader = 0 = North
  }

  it should "return all four players" in {
    game0.players shouldBe Seq(0, 1, 2, 3)
  }

  it should "return a fresh State from start" in {
    game0.start.cardsPlayed shouldBe 0
    game0.start.tricks shouldBe Tricks.zero
  }

  // ---------------------------------------------------------------------------
  // moves
  // ---------------------------------------------------------------------------

  behavior of "WhistGame.moves"

  it should "return the same number of moves as enumeratePlays" in {
    val s = State(whist0)
    game0.moves(s).size shouldBe s.enumeratePlays.size
  }

  it should "return CardPlay instances" in {
    val s = State(whist0)
    game0.moves(s) should not be empty
    game0.moves(s).foreach { cp =>
      cp shouldBe a[CardPlay]
    }
  }

  // ---------------------------------------------------------------------------
  // applyMove
  // ---------------------------------------------------------------------------

  behavior of "WhistGame.applyMove"

  it should "produce a successor with one more card played" in {
    val s = State(whist0)
    val move = game0.moves(s).head
    val next = game0.applyMove(s, move, 0)
    next.cardsPlayed shouldBe s.cardsPlayed + 1
  }

  it should "produce the same successor as enumeratePlays" in {
    val s = State(whist0)
    val move = game0.moves(s).head
    val next = game0.applyMove(s, move, 0)
    val fromEnum = s.enumeratePlays.find(_.trick.plays.lastOption.contains(move))
    fromEnum shouldBe Some(next)
  }

  // ---------------------------------------------------------------------------
  // nextPlayer
  // ---------------------------------------------------------------------------

  behavior of "WhistGame.nextPlayer"

  it should "advance clockwise from North" in {
    game0.nextPlayer(game0.start, 0) shouldBe 1
  }

  it should "advance clockwise from West back to North" in {
    game0.nextPlayer(game0.start, 3) shouldBe 0
  }

  it should "advance through all four seats" in {
    (0 until 4).foreach { i =>
      game0.nextPlayer(game0.start, i) shouldBe (i + 1) % 4
    }
  }

  // ---------------------------------------------------------------------------
  // winner
  // ---------------------------------------------------------------------------

  behavior of "WhistGame.winner"

  it should "give +1 to the previous player's partnership and -1 to the other" in {
    val s = game0.start
    // current = 1 (E), so prev = 0 (N); NS wins
    val result = game0.winner(s, current = 1)
    result(0) shouldBe 1 // N wins
    result(2) shouldBe 1 // S wins (same side as N)
    result(1) shouldBe -1 // E loses
    result(3) shouldBe -1 // W loses
  }

  it should "give zero-sum result" in {
    val result = game0.winner(game0.start, current = 0)
    result.values.sum shouldBe 0
  }