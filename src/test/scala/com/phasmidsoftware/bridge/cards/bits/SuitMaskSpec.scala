/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import org.scalatest.flatspec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class SuitMaskSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "SuitMask basic ops"

  it should "set, clear, and test ranks" in {
    val m = SuitMask.empty.setRank(3).setRank(7)
    m.contains(3) shouldBe true
    m.contains(7) shouldBe true
    m.contains(0) shouldBe false
    m.clearRank(3).contains(3) shouldBe false
  }

  it should "compute topBit and topRank" in {
    val m = SuitMask.rank(2).union(SuitMask.rank(9)).union(SuitMask.rank(5))
    m.topBit shouldBe SuitMask.rank(9)
    m.topRank shouldBe 9
  }

  it should "report size and emptiness" in {
    SuitMask.empty.isEmpty shouldBe true
    SuitMask.empty.size shouldBe 0
    SuitMask.rank(0).union(SuitMask.rank(1)).size shouldBe 2
  }

  it should "list ranks highest-first" in {
    val m = SuitMask.rank(1).union(SuitMask.rank(4)).union(SuitMask.rank(10))
    m.ranks shouldBe List(10, 4, 1)
  }

  it should "support union, intersect, and diff" in {
    val a = SuitMask.rank(1).union(SuitMask.rank(2))
    val b = SuitMask.rank(2).union(SuitMask.rank(3))
    a.union(b) shouldBe SuitMask.rank(1).union(SuitMask.rank(2)).union(SuitMask.rank(3))
    a.intersect(b) shouldBe SuitMask.rank(2)
    a.diff(b) shouldBe SuitMask.rank(1)
  }

  behavior of "SuitMask.equivalenceClasses"

  it should "treat a hand's whole holding as one class when no opponent cards remain" in {
    // hand holds A(12), K(11), Q(10); opponents hold nothing in this suit
    val hand = SuitMask.rank(12).union(SuitMask.rank(11)).union(SuitMask.rank(10))
    val opponents = SuitMask.empty
    SuitMask.equivalenceClasses(hand, opponents).toList shouldBe List(hand)
  }

  it should "NOT split a class when the intervening card belongs to partner (not passed as opponentBits)" in {
    // hand holds A(12) and Q(10); the K(11) "in between" belongs to partner, so it must
    // NOT be included in opponentBits -- callers pass only the opposing side's mask.
    val hand = SuitMask.rank(12).union(SuitMask.rank(10))
    val opponents = SuitMask.empty // partner's K is deliberately absent here
    SuitMask.equivalenceClasses(hand, opponents).toList shouldBe List(hand)
  }

  it should "split a class when an opponent card lies between two of the hand's cards" in {
    // hand holds A(12) and Q(10); opponent holds K(11) in between -- A and Q are NOT equivalent
    val hand = SuitMask.rank(12).union(SuitMask.rank(10))
    val opponents = SuitMask.rank(11)
    SuitMask.equivalenceClasses(hand, opponents).toSet shouldBe Set(SuitMask.rank(12), SuitMask.rank(10))
  }

  it should "merge two runs once the opponent's separating card is gone (promotion)" in {
    // Before: hand holds A(12), Q(10); opponent holds K(11) => two singleton classes.
    // After the K is played (removed from opponents' mask): they merge into one class.
    val hand = SuitMask.rank(12).union(SuitMask.rank(10))
    val beforeOpponents = SuitMask.rank(11)
    val afterOpponents = SuitMask.empty
    SuitMask.equivalenceClasses(hand, beforeOpponents).toSet shouldBe Set(SuitMask.rank(12), SuitMask.rank(10))
    SuitMask.equivalenceClasses(hand, afterOpponents).toList shouldBe List(hand)
  }

  it should "produce one class per opponent-free interval for a longer holding" in {
    // hand holds ranks 12,11,9,8,5; opponents hold 10 and 6 (breaking the run twice)
    val hand = List(12, 11, 9, 8, 5).foldLeft(SuitMask.empty)((m, r) => m.union(SuitMask.rank(r)))
    val opponents = SuitMask.rank(10).union(SuitMask.rank(6))
    val classes = SuitMask.equivalenceClasses(hand, opponents)
    classes.toSet shouldBe Set(
      SuitMask.rank(12).union(SuitMask.rank(11)),
      SuitMask.rank(9).union(SuitMask.rank(8)),
      SuitMask.rank(5)
    )
  }

  it should "return no classes when the hand is void in this suit" in {
    SuitMask.equivalenceClasses(SuitMask.empty, SuitMask.rank(5)) shouldBe empty
  }

  behavior of "SuitMask.compact"

  it should "map the whole universe onto a dense 0-based run, preserving relative order" in {
    // universe = ranks 3,7,9,12 (four live cards); mask = the same four cards (the whole universe)
    val universe = List(3, 7, 9, 12).foldLeft(SuitMask.empty)((m, r) => m.union(SuitMask.rank(r)))
    SuitMask.compact(universe, universe) shouldBe List(0, 1, 2, 3).foldLeft(SuitMask.empty)((m, r) => m.union(SuitMask.rank(r)))
  }

  it should "map a subset of the universe to the canonical positions of just those ranks" in {
    // universe = 3,7,9,12 (canonical positions 0,1,2,3 respectively); mask = just 7 and 12
    val universe = List(3, 7, 9, 12).foldLeft(SuitMask.empty)((m, r) => m.union(SuitMask.rank(r)))
    val mask = SuitMask.rank(7).union(SuitMask.rank(12))
    SuitMask.compact(mask, universe) shouldBe SuitMask.rank(1).union(SuitMask.rank(3))
  }

  it should "give the same canonical result for two differently-absolute but same-shaped suits" in {
    // "9-7-5-3 with everything else gone" and "A-K-Q-J with everything else gone" are the
    // same shape: four consecutive live ranks, hand holds the top two of them.
    val universeLow = List(3, 5, 7, 9).foldLeft(SuitMask.empty)((m, r) => m.union(SuitMask.rank(r)))
    val handLow = SuitMask.rank(7).union(SuitMask.rank(9))
    val universeHigh = List(9, 10, 11, 12).foldLeft(SuitMask.empty)((m, r) => m.union(SuitMask.rank(r)))
    val handHigh = SuitMask.rank(11).union(SuitMask.rank(12))
    SuitMask.compact(handLow, universeLow) shouldBe SuitMask.compact(handHigh, universeHigh)
  }

  it should "handle a singleton universe" in {
    val universe = SuitMask.rank(6)
    SuitMask.compact(universe, universe) shouldBe SuitMask.rank(0)
  }
}
