/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Seconds, Span}
import org.scalatest.flatspec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class WhistFuncSpec extends flatspec.AnyFlatSpec with should.Matchers with TimeLimitedTests {

  val timeLimit = Span(2, Seconds)

  /** Assert only on the makes field, ignoring tricks depth. */
  private def assertMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case DDResult.Partial(makes, _) => makes shouldBe expected
      case DDResult.Inconclusive => fail(s"Expected makes=$expected but got Inconclusive")

  behavior of "double dummy"
  // 1.5 seconds
  ignore should "analyzeDoubleDummy0" in {
    val target = Deal.createRandom("test", 0L, adjustForPartnerships = false)
    val whist = Whist(target, 0)
    assertMakes(whist.analyzeDoubleDummy(9, directionNS = false), true)
  }
}
