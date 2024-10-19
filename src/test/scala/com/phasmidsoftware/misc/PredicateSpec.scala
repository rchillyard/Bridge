/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.misc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PredicateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Predicate"

  private val isEven: IntPredicate = IntPredicate("even", _ % 2 == 0)
  private val isOdd: IntPredicate = IntPredicate("odd", _ % 2 != 0)
  private val isPositive: IntPredicate = IntPredicate("pos", _ > 0)

  it should "get lens right" in {
    isEven.lens[Int](_ + 1)(1) shouldBe true
    isEven.lens[Int](_ + 1)(0) shouldBe false
    isEven.lens[Int](_ * 2)(1) shouldBe true
    isEven.lens[Int](_ * 2)(0) shouldBe true
  }

  it should "get flip right" in {
    isEven.flip(1) shouldBe true
    isOdd.flip(2) shouldBe true
  }

  it should "get andThen right" in {
    (isOdd andThen isPositive)(1) shouldBe true
    (isOdd andThen isPositive)(-1) shouldBe false
    (isOdd andThen isPositive)(2) shouldBe false
    (isOdd andThen isPositive)(-2) shouldBe false
  }

  it should "get orElse right" in {
    (isOdd orElse isPositive)(1) shouldBe true
    (isOdd orElse isPositive)(-1) shouldBe true
    (isOdd orElse isPositive)(2) shouldBe true
    (isOdd orElse isPositive)(-2) shouldBe false
  }

  it should "get implies right" in {
    (isOdd implies isPositive)(1) shouldBe true
    (isOdd implies isPositive)(-1) shouldBe false
    (isOdd implies isPositive)(2) shouldBe true
    (isOdd implies isPositive)(-2) shouldBe true
  }
}

case class IntPredicate(name: String, f: Int => Boolean) extends BasePredicate[Int](name, f)
//  def apply(x: Int): Boolean = { val result = f(x); if (result) println(s"$name matched"); result }

