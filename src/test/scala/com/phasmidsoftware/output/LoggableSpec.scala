/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import com.phasmidsoftware.bridge.cards.{Deal, State, Whist}
import com.phasmidsoftware.util.Loggable
import org.scalatest.{FlatSpec, Matchers}

/**
  * This adds one test to LoggableSpec in DecisionTree.
  */
//noinspection ScalaStyle
class LoggableSpec extends FlatSpec with Matchers {

  behavior of "Loggable"

  it should "toLog State" in {
    val whist = Whist(Deal("0", 0L), 0)
    implicitly[Loggable[State]].toLog(State(whist)) shouldBe "T0  {} 0:0 3.9 Deal 0/52@N:NT"
  }
}
