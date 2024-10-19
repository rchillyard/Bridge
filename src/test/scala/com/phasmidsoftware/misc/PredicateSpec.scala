/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.misc

import com.phasmidsoftware.misc.Predicate.{isEven, isOdd, isPositive}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PredicateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "IntPredicate"

  it should "apply" in {
    isEven(2) shouldBe true
    isEven(1) shouldBe false
  }

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

  behavior of "BooleanIntPredicate"

  it should "apply" in {
    val controlPredicate: Boolean => Predicate[Int] = BooleanIntPredicate("boolean control", r => t => if (r) isEven(t) else isOdd(t))
    val evenPredicate = controlPredicate(true)
    val oddPredicate = controlPredicate(false)
    evenPredicate(2) shouldBe true
    evenPredicate(1) shouldBe false
    oddPredicate(2) shouldBe false
    oddPredicate(1) shouldBe true
  }

}

case class IntPredicate(name: String, f: Int => Boolean) extends BasePredicate[Int](name, f)

case class BooleanIntPredicate(name: String, f: Boolean => Int => Boolean)(b: Boolean) extends BaseControlPredicate[Boolean, Int](name, f)(b)