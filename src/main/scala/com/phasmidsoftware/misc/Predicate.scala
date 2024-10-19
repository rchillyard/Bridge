/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.misc

import scala.language.implicitConversions

trait Predicate[T] extends (T => Boolean) {
  self =>

  def lens[U](f: U => T): Predicate[U] = (u: U) => self(f(u))

  def flip: Predicate[T] = (t: T) => !self(t)

  def andThen(p: Predicate[T]): Predicate[T] = (t: T) => self(t) && p(t)

  def orElse(p: Predicate[T]): Predicate[T] = (t: T) => self(t) || p(t) //(flip andThen p.flip).flip

  def implies(p: Predicate[T]): Predicate[T] = (t: T) => if (self(t)) p(t) else true
}

/**
  * NOTE: strictly speaking, this does not need to be abstract.
  *
  * @param name        the name of the predicate.
  * @param f           the predicate function.
  * @param showMatches whether to show matches (default should be false).
  * @tparam T the underlying type.
  */
abstract class BasePredicate[T](name: String, f: T => Boolean, showMatches: Boolean = false) extends Predicate[T] {
  override def apply(t: T): Boolean = {
    val result = f(t)
    if (result && showMatches) println(s"$name matched for t=$t")
    result
  }
}

/**
  * NOTE: strictly speaking, this does not need to be abstract.
  *
  * @param name        the name of the predicate.
  * @param f           a function wuch that a value of r will yield the predicate function.
  * @param showMatches whether to show matches (default should be false).
  * @tparam R the underlying type of the control.
  * @tparam T the underlying type of the returned Predicate.
  */
abstract class BaseControlPredicate[R, T](name: String, f: R => T => Boolean, showMatches: Boolean = false)(r: R) extends Predicate[T] {
  override def apply(t: T): Boolean = {
    val result = f(r)(t)
    if (result && showMatches) println(s"$name(r=$r) matched for t=$t")
    result
  }
}

case class IntPredicate(name: String, f: Int => Boolean) extends BasePredicate[Int](name, f)

object Predicate {
  implicit def intToCompound(x: Int): Compound = Compound(x)

  val isEven: Predicate[Int] = IntPredicate("even", _ :| 2)
  val isOdd: Predicate[Int] = isEven.flip
  val isPositive: Predicate[Int] = IntPredicate("pos", _ > 0)
}

case class Compound(x: Int) {
  def :|(y: Int): Boolean = x % y == 0
}