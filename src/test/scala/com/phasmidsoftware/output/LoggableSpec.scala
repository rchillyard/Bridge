/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import com.phasmidsoftware.bridge.cards.{Deal, State, Whist}
import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class LoggableSpec extends FlatSpec with Matchers {

  behavior of "Loggable"

  it should "toLog" in {
    implicitly[Loggable[Boolean]].toLog(true) shouldBe "true"
    implicitly[Loggable[Int]].toLog(42) shouldBe "42"
    implicitly[Loggable[Double]].toLog(42.0) shouldBe "42.0"
    implicitly[Loggable[String]].toLog("42") shouldBe "42"
  }

  it should "toLog State" in {
    val whist = Whist(Deal("0", 0L), 0)
    implicitly[Loggable[State]].toLog(State(whist)) shouldBe "T0 {} 0:0 3.9 Deal 0/52@N"
  }
}
