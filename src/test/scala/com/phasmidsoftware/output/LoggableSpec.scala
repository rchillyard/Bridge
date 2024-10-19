/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import com.phasmidsoftware.bridge.cards.{Deal, State, Whist}
import com.phasmidsoftware.flog.Loggable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * This adds one test to LoggableSpec in DecisionTree.
  */
//noinspection ScalaStyle
class LoggableSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Loggable"

  it should "toLog State" in {
    val whist = Whist(Deal("0", 0L), 0)
    // NOTE: changed logging mechanism. This was previously: T0  {} 0:0 4.0 Deal 0/52@N:NT
    implicitly[Loggable[State]].toLog(State(whist)) shouldBe "State(Whist(Deal 0 (52 cards and 40 sequences), N, NT),T0  {},0:0)"
  }
}
