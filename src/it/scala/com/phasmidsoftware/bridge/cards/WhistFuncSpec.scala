/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

//noinspection ScalaStyle
class WhistSpec extends FlatSpec with Matchers {

  behavior of "double dummy"
  it should "analyzeDoubleDummy0" in {
    val target = Deal("test", 0L)
    val whist = Whist(target, 0)
    whist.analyzeDoubleDummy(9, directionNS = false) shouldBe Some(true)
  }
}
