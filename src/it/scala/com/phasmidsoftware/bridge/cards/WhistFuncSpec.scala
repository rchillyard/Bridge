/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class WhistFuncSpec extends FlatSpec with Matchers with TimeLimitedTests {

  val timeLimit = Span(2, Seconds)

  behavior of "double dummy"
  // 1.5 seconds
  it should "analyzeDoubleDummy0" in {
    val target = Deal("test", 0L, adjustForPartnerships = false)
    val whist = Whist(target, 0)
    whist.analyzeDoubleDummy(9, directionNS = false) shouldBe Some(true)
  }
}
