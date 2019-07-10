/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import java.io.OutputStream

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class SmartValueOpsSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  override def beforeEach() {
    SmartValueOps.setInvariantsEnabled(true)
  }

  override def afterEach() {
  }

  import SmartValueOps._

  behavior of "invariant"

  it should "apply with StringBuilderOutputStream" in {
    val outputStream = new StringBuilderOutputStream()
    Console.withOut(outputStream)((Math.PI * 2).invariant(x => x > 0, "x should be positive")) shouldBe Math.PI * 2
    Console.withOut(outputStream)((Math.PI * 2).invariant(x => x < 0, "x should be negative")) shouldBe Math.PI * 2
    outputStream.toString() shouldBe "x should be negative: 6.283185307179586\n"
  }

  it should "apply with logging" in {
    val logger = MockLogger("SmartValue")
    (Math.PI * 2).invariant(x => x > 0, logger, "x should be positive") shouldBe Math.PI * 2
    (Math.PI * 2).invariant(x => x < 0, logger,"x should be negative") shouldBe Math.PI * 2
    logger.toString shouldBe "SmartValue: DEBUG: x should be negative: 6.283185307179586\n"
  }

  it should "apply with exception" in {
    (Math.PI * 2).invariant(x => x > 0) shouldBe Math.PI * 2
    an[Exception] shouldBe thrownBy ((Math.PI * 2).invariant(x => x < 0))
  }

  behavior of "invariant turned off"

  it should "apply with StringBuilderOutputStream" in {
    SmartValueOps.setInvariantsEnabled(false)
    val outputStream = new StringBuilderOutputStream()
    Console.withOut(outputStream)((Math.PI * 2).invariant(x => x > 0, "x should be positive")) shouldBe Math.PI * 2
    Console.withOut(outputStream)((Math.PI * 2).invariant(x => x < 0, "x should be negative")) shouldBe Math.PI * 2
    outputStream.toString() shouldBe ""
  }

  it should "apply with logging" in {
    SmartValueOps.setInvariantsEnabled(false)
    val logger = MockLogger("SmartValue")
    (Math.PI * 2).invariant(x => x > 0, logger, "x should be positive") shouldBe Math.PI * 2
    (Math.PI * 2).invariant(x => x < 0, logger,"x should be negative") shouldBe Math.PI * 2
    logger.toString shouldBe ""
  }

  it should "apply with exception" in {
    SmartValueOps.setInvariantsEnabled(false)
    (Math.PI * 2).invariant(x => x > 0) shouldBe Math.PI * 2
    (Math.PI * 2).invariant(x => x < 0) shouldBe Math.PI * 2
  }



}
