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

  def orElse(p: Predicate[T]): Predicate[T] = (flip andThen p.flip).flip

  def implies(p: Predicate[T]): Predicate[T] = (t: T) => if (self(t)) p(t) else true
}

class BasePredicate[T](name: String, f: T => Boolean, showMatches: Boolean = true) extends Predicate[T] {
  override def apply(t: T): Boolean = if (f(t)) {
    if (showMatches) println(s"$name matched for t=$t");
    true
  } else false
}

case class IntPredicate(name: String, f: Int => Boolean) extends BasePredicate[Int](name, f)

object Predicate {
  implicit def intToCompound(x: Int): Compound = Compound(x)
}

case class Compound(x: Int) {
  def :|(y: Int): Boolean = x % y == 0
}