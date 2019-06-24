/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class TricksSpec extends FlatSpec with Matchers {

  behavior of "TricksSpec"

  it should "project" in {
    val target = Tricks(1, 2)
    target.project(directionNS = true) shouldBe target
    target.project(directionNS = false) shouldBe Tricks(2, 1)
  }

  it should "increment" in {
    val target = Tricks.zero
    target.increment(0) shouldBe Tricks(1, 0)
    target.increment(1) shouldBe Tricks(0, 1)
    target.increment(2) shouldBe Tricks(1, 0)
    target.increment(3) shouldBe Tricks(0, 1)
  }

  it should "goal" in {
    val target = Tricks(12, 2)
    target.goal(12) shouldBe true
    target.goal(11) shouldBe true
    target.goal(13) shouldBe false
  }

  it should "counterGoal" in {
    val target = Tricks(12, 2)
    target.counterGoal(12) shouldBe true
    target.counterGoal(11) shouldBe false
    target.counterGoal(13) shouldBe true
  }

  it should "decide(Int)" in {
    Tricks(12, 2).decide(12) shouldBe Some(true)
    Tricks(12, 2).decide(13) shouldBe Some(false)
    Tricks(12, 0).decide(13) shouldBe None
  }

  it should "decide(Int,Boolean" in {
    Tricks(12, 2).decide(12, directionNS = true) shouldBe Some(true)
    Tricks(12, 2).decide(13, directionNS = true) shouldBe Some(false)
    Tricks(12, 0).decide(13, directionNS = true) shouldBe None
    Tricks(2, 12).decide(12, directionNS = false) shouldBe Some(true)
    Tricks(2, 12).decide(13, directionNS = false) shouldBe Some(false)
    Tricks(0, 12).decide(13, directionNS = false) shouldBe None
  }

  it should "incNS" in {
    val target = Tricks.zero
    target.incNS shouldBe Tricks(1, 0)
  }

  it should "incEW" in {
    val target = Tricks.zero
    target.incEW shouldBe Tricks(0, 1)
  }

}
