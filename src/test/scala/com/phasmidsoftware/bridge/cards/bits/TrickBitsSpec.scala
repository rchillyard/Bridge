/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import com.phasmidsoftware.bridge.cards.Strain
import org.scalatest.flatspec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class TrickBitsSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "TrickBits.score tiers"

  it should "always score a followed-suit play above any off-suit discard, regardless of rank" in {
    // worst possible follow (rank 0) must still beat the best possible off-suit discard (rank 12)
    TrickBits.score(rank = 0, followsSuit = true, isRuff = false) should be > TrickBits.score(rank = 12, followsSuit = false, isRuff = false)
  }

  it should "always score a ruff above any followed-suit play, regardless of rank" in {
    // worst possible ruff (rank 0) must still beat the best possible follow (rank 12)
    TrickBits.score(rank = 0, followsSuit = false, isRuff = true) should be > TrickBits.score(rank = 12, followsSuit = true, isRuff = false)
  }

  it should "rank within a tier by card rank" in {
    TrickBits.score(rank = 9, followsSuit = true, isRuff = false) should be > TrickBits.score(rank = 3, followsSuit = true, isRuff = false)
    TrickBits.score(rank = 9, followsSuit = false, isRuff = true) should be > TrickBits.score(rank = 3, followsSuit = false, isRuff = true)
  }

  behavior of "TrickBits.winningPlay"

  it should "pick the highest card in the led suit when nobody ruffs or discards higher" in {
    val plays = Seq(
      TrickPlay(handIndex = 0, suitIndex = 0, rank = 12), // led suit, Ace
      TrickPlay(handIndex = 1, suitIndex = 0, rank = 5), // led suit, low
      TrickPlay(handIndex = 2, suitIndex = 1, rank = 12), // off-suit discard, even though it's the "Ace" of another suit
      TrickPlay(handIndex = 3, suitIndex = 0, rank = 8) // led suit, mid
    )
    TrickBits.winningPlay(plays, ledSuit = 0, strain = Strain.NoTrump) shouldBe plays.head
  }

  it should "let even the lowest trump beat the highest card of the led suit" in {
    val plays = Seq(
      TrickPlay(handIndex = 0, suitIndex = 0, rank = 12), // led suit, Ace
      TrickPlay(handIndex = 1, suitIndex = 1, rank = 0), // ruffed with the lowest trump
      TrickPlay(handIndex = 2, suitIndex = 0, rank = 8),
      TrickPlay(handIndex = 3, suitIndex = 0, rank = 5)
    )
    TrickBits.winningPlay(plays, ledSuit = 0, strain = Strain(1)) shouldBe plays(1)
  }

  it should "not treat an off-suit play in the trump suit as a ruff when it's actually the led suit" in {
    // trumpSuit == ledSuit (a trump contract where trump was led): should score as followSuit, not ruff
    val plays = Seq(
      TrickPlay(handIndex = 0, suitIndex = 0, rank = 12),
      TrickPlay(handIndex = 1, suitIndex = 0, rank = 5)
    )
    TrickBits.winningPlay(plays, ledSuit = 0, strain = Strain(0)) shouldBe plays(0)
  }

  it should "let the higher trump win when both defenders ruff" in {
    val plays = Seq(
      TrickPlay(handIndex = 0, suitIndex = 0, rank = 12),
      TrickPlay(handIndex = 1, suitIndex = 1, rank = 3),
      TrickPlay(handIndex = 2, suitIndex = 1, rank = 7)
    )
    TrickBits.winningPlay(plays, ledSuit = 0, strain = Strain(1)) shouldBe plays(2)
  }
}
