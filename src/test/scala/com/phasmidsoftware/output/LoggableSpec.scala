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
    val whist = Whist(Deal.createRandom("0", 0L), 0)
    implicitly[Loggable[State]].toLog(State(whist)) shouldBe "State: Trick History: \"T0  {}\" 0:0 -2.2 Deal 0 (52) List(S95 HQ9432 D64 CT652, SK742 HA7 DT93 CAQJ7, SAJT86 HKT8 DK82 CK3, SQ3 HJ65 DAQJ75 C984)"
  }
}
