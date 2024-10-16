/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrioritySpec extends AnyFlatSpec with should.Matchers {

  behavior of "priority"

  it should "be 0 for spades, etc." in {
    Spades.priority shouldBe 0
    Clubs.priority shouldBe 3
  }

  it should "be 0 for ace, etc." in {
    Ace.priority shouldBe 0
    Deuce.priority shouldBe 12
  }
}
