/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import java.io.OutputStream

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import org.slf4j.LoggerFactory

class SmartValueOpsSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  override def beforeEach() {
    SmartValueOps.setEnabled(true)
  }

  override def afterEach() {
  }

  import SmartValueOps._

  behavior of "invariant"

  it should "apply with Console" in {
    (Math.PI * 2).invariant(x => x > 0, "x should be positive") shouldBe Math.PI * 2
    // NOTE: the following should result in an entry on the Console
    (Math.PI * 2).invariant(x => x < 0, "x should be negative") shouldBe Math.PI * 2
  }

  it should "apply with StringBuilder" in {
    val sb = new StringBuilder()
    val outputStream = new OutputStream {
      def write(b: Int): Unit = sb.append(b.toChar)
    }
    Console.withOut(outputStream)((Math.PI * 2).invariant(x => x < 0, "x should be negative"))
    sb.toString() shouldBe "x should be negative: 6.283185307179586\n"
  }

  it should "apply with logging" in {
    val logger = LoggerFactory.getLogger("SmartValue")
    (Math.PI * 2).invariant(x => x > 0, logger, "x should be positive") shouldBe Math.PI * 2
    // NOTE: the following should result in an entry in the log for the SmartValue class
    (Math.PI * 2).invariant(x => x < 0, logger,"x should be negative") shouldBe Math.PI * 2
  }

  it should "apply with exception" in {
    (Math.PI * 2).invariant(x => x > 0) shouldBe Math.PI * 2
    an[Exception] shouldBe thrownBy ((Math.PI * 2).invariant(x => x < 0))
  }

  behavior of "invariant turned off"

  it should "apply with Console" in {
    SmartValueOps.setEnabled(false)

    (Math.PI * 2).invariant(x => x > 0, "x should be positive") shouldBe Math.PI * 2
    // NOTE: the following should NOT result in an entry on the Console
    (Math.PI * 2).invariant(x => x < 0, "x should be negative") shouldBe Math.PI * 2
  }

  it should "apply with StringBuilder" in {
    SmartValueOps.setEnabled(false)
    val sb = new StringBuilder()
    val outputStream = new OutputStream {
      def write(b: Int): Unit = sb.append(b.toChar)
    }
    Console.withOut(outputStream)((Math.PI * 2).invariant(x => x < 0, "x should be negative"))
    sb.toString() shouldBe ""
  }

  it should "apply with logging" in {
    SmartValueOps.setEnabled(false)
    val logger = LoggerFactory.getLogger("SmartValue")
    (Math.PI * 2).invariant(x => x > 0, logger, "x should be positive") shouldBe Math.PI * 2
    // NOTE: the following should NOT result in an entry in the log for the SmartValue class
    (Math.PI * 2).invariant(x => x < 0, logger,"x should be negative") shouldBe Math.PI * 2
  }

  it should "apply with exception" in {
    SmartValueOps.setEnabled(false)
    (Math.PI * 2).invariant(x => x > 0) shouldBe Math.PI * 2
    (Math.PI * 2).invariant(x => x < 0) shouldBe Math.PI * 2
  }



}
