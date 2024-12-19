/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.misc

import scala.annotation.unused
import scala.language.reflectiveCalls // XXX what are these?

/**
  * Trait to describe the behavior of a predicate.
  * Predicate extends (T => Boolean) so is a pure function that implements the `apply(t)` method.
  *
  * @tparam T the underlying type, that's to say the type of the input to `this` Predicate.
  */
trait Predicate[T] extends (T => Boolean) {

  self =>

  /**
    * Method to transform `this` Predicate[T] into a Predicate[U].
    *
    * @param f a function which takes a U and returns a T.
    * @tparam U the underlying type of the result.
    * @return a Predicate[U].
    */
  def lens[U](f: U => T): Predicate[U] = (u: U) => self(f(u))

  /**
    * Method to reverse the sense of this Predicate[T].
    *
    * @return a Predicate[T] that returns true when `this` Predicate would return false; and false when `this` would return true.
    */
  def flip: Predicate[T] = (t: T) => !self(t)

  /**
    * Method to compose a Predicate[T] from `this` and `p`.
    *
    * @param p a Predicate[T]
    * @return a Predicate[T] which will yield true for a `t` value only when `this` and `p` yield true.
    *         NOTE that `p` will not be invoked if `this` yields false.
    */
  def andThen(p: Predicate[T]): Predicate[T] = (t: T) => self(t) && p(t)

  /**
    * Method to compose a Predicate[T] from `this` or `p`.
    *
    * @param p a Predicate[T]
    * @return a Predicate[T] which will yield true for a `t` value when `this` or `p` yield true.
    *         NOTE that `p` will not be invoked if `this` yields true.
    */
  def orElse(p: Predicate[T]): Predicate[T] = (t: T) => self(t) || p(t)

  /**
    * Method to compose a Predicate[T] from `this` implies `p`.
    *
    * @param p a Predicate[T]
    * @return a Predicate[T] which will yield true for a `t` value when `this` implies `p` yields true.
    *         NOTE that `p` will not be invoked if `this` yields true.
    */
  def implies(p: Predicate[T]): Predicate[T] = (t: T) => if (self(t)) p(t) else true

}

/**
  * Trait that extends Predicate[T] but with the added behavior of providing a justification when the predicate succeeds.
  *
  * @tparam T the underlying type, that's to say the type of the input to `this JPredicate`.
  */
trait JPredicate[T] extends Predicate[T] {

  self =>

  /**
    * Method which yields an optional `String` based on the input value `t`.
    *
    * @param t a value of type `T`.
    * @return an `Option[String]`.
    */
  def justification(t: T): Option[String]

  /**
    * The `apply` method for this predicate.
    *
    * @param t a value of type `T`.
    * @return a Boolean: true if `justification(t)` is defined.
    */
  def apply(t: T): Boolean = justification(t).isDefined

  /**
    * Method to reverse the sense of this Predicate[T].
    *
    * CONSIDER re-writing.
    *
    * @return a Predicate[T] that returns true when `this` Predicate would return false;
    *         and false when `this` would return true.
    */
  override def flip: JPredicate[T] = (t: T) => self.justification(t) match {
    case None => Some("not")
    case _ => None
  }

  /**
    * Method to transform `this` JPredicate[T] into a JPredicate[U].
    *
    * @param f a function which takes a U and returns a T.
    * @param w a String which will be the justification, if any (currently, independent of any actual `T` value).
    * @tparam U the underlying type of the result.
    * @return a JPredicate[U].
    */
  def jLens[U](w: => String)(f: U => T): JPredicate[U] = (u: U) => self.justification(f(u)) map (w + " " + _)

  /**
    * Method to compose a JPredicate[T] from `this` or `p`.
    *
    * CONSIDER re-writing this method
    *
    * @param p a Predicate[T]
    * @return a Predicate[T] which will yield true for a `t` value when `this` or `p` yield true.
    *         NOTE that `p` will not be invoked if `this` yields true.
    */
  override def orElse(p: Predicate[T]): JPredicate[T] =
    (t: T) => self.justification(t) orElse (p match {
      case j: JPredicate[T] => j.justification(t)
      case _ if p(t) => Some("orElse")
      case _ => None
    })

  /**
    * Method to compose a Predicate[T] from `this` and `p`.
    *
    * @param p a `Predicate[T]`
    * @return a `JPredicate[T]` which will yield true for a `t` value only when `this` and `p` yield true.
    *         NOTE that `p` will not be invoked if `this` yields false.
    */
  override def andThen(p: Predicate[T]): JPredicate[T] =
    (t: T) => self.justification(t) flatMap (
      w1 =>
        p match {
          case j: JPredicate[T] => j.justification(t) map (w2 => w1 + "&" + w2)
          case _ if p(t) => Some(w1)
          case _ => None
        })
}

object JPredicate {
  def apply[T](f: T => Option[String]): JPredicate[T] = (t: T) => f(t)

  def when[T](f: T => String)(p: T => Boolean): JPredicate[T] = apply(t => Option.when(p(t))(f(t)))
}

import com.phasmidsoftware.misc.Predicate.maybeShow

import scala.language.reflectiveCalls

/**
  * NOTE: strictly speaking, this does not need to be abstract.
  *
  * @param name        the name of the predicate.
  * @param f           the predicate function.
  * @param showMatches whether to show matches (default should be false).
  * @tparam T the underlying type.
  */
abstract class BasePredicate[T](name: String, f: T => Boolean, showMatches: Boolean = false) extends Predicate[T] {
  def apply(t: T): Boolean = maybeShow(f(t), s"$name matched for sb=$t", showMatches)
}

/**
  * NOTE: strictly speaking, this does not need to be abstract.
  *
  * @param name        the name of the predicate.
  * @param f           a function such that a value of r will yield the predicate function.
  * @param showMatches whether to show matches (default should be false).
  * @tparam R the underlying type of the control.
  * @tparam T the underlying type of the returned Predicate.
  */
abstract class BaseControlPredicate[R, T](name: String, f: R => T => Boolean, showMatches: Boolean = false)(r: R) extends Predicate[T] {
  def apply(t: T): Boolean = maybeShow(f(r)(t), s"$name(r=$r) matched for sb=$t", showMatches)
}

/**
  * Case class representing a concrete Predicate[Int].
  *
  * @param name the name of the predicate.
  * @param f    the predicate function.
  */
case class IntPredicate(name: String, f: Int => Boolean) extends BasePredicate[Int](name, f) {
  /**
    * Method to copy this Predicate but with a different name.
    *
    * @param w a String to be used as the new name.
    * @return a IntPredicate that behaves the same as `this` but with a different name in the case of a positive match.
    */
  @unused
  def named(w: String): IntPredicate = copy(name = w)
}

/**
  * Companion object to the trait Predicate.
  *
  */
object Predicate {

  /**
    * Apply method to construct a Predicate[T] from a function `f` of type T => Boolean.
    *
    * @param f the function which will be used for the resulting Predicate[T].
    * @tparam T the underlying type.
    * @return a Predicate[T] that will not extend BasePredicate[T] amd so will never print matches.
    */
  def apply[T](f: T => Boolean): Predicate[T] = (t: T) => f(t)

  lazy val isEven: Predicate[Int] = IntPredicate("even", _ :| 2)

  lazy val isOdd: Predicate[Int] = IntPredicate("odd", isEven.flip)

  lazy val isPositive: Predicate[Int] = IntPredicate("pos", _ > 0)

  /**
    * Return the value of `b` and (as a side effect), if it is true and if `show` is true, then print the `msg`.
    *
    * @param b    a Boolean.
    * @param msg  a String to be conditionally printed.
    * @param show if true (and if `b` is true) then print `msg`.
    * @return `b`.
    */
  def maybeShow(b: Boolean, msg: => String, show: Boolean): Boolean = {
    if (b && show) println(msg)
    b
  }

  /**
    * Implicit class to represent a compound number (i.e., not prime).
    *
    * @param x the value of the number.
    */
  implicit class Compound(x: Int) {
    /**
      * Method to determine if `y` is a factor of `x`.
      *
      * @param y an Int.
      * @return a Boolean.
      */
    def :|(y: Int): Boolean = x % y == 0
  }
}

