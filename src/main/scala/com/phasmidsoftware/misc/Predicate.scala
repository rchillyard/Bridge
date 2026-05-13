/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.misc

import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.misc.Predicate.NamedFunction

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

  val flog = Flog[JPredicate[?]].disabled

  import flog._

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
  override def flip: JPredicate[T] = (t: T) => s"flip $t $self" !! self.justification(t) match {
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
  def jLens[U](w: String)(f: U => T): JPredicate[U] = (u: U) => s"jLens($w, $f): $u" !! self.justification(f(u)) map (w + " " + _)

  /**
    * Method to transform `this` JPredicate[T] into a JPredicate[U].
    *
    * @param f a function which takes a U and returns a T.
    * @param w a String which will be the justification, if any (currently, independent of any actual `T` value).
    * @tparam U the underlying type of the result.
    * @return a JPredicate[U].
    */
  def jLensOpt[U](w: String)(f: U => Option[T]): JPredicate[U] = (u: U) =>
    s"jLensOpt($w, $f): $u" !! (f(u) flatMap self.justification map (w + " " + _))

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
    (t: T) => s"orElse($p): $t" !! self.justification(t) orElse (p match {
      case j: JPredicate[T] => s"orElse JPredicate $t" !! j.justification(t)
      case _ if p(t) => s"orElse Predicate $t" !! Some("orElse")
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
    (t: T) => s"andThen($p): $t" !! self.justification(t) flatMap (
      w1 =>
        p match {
          case j: JPredicate[T] =>
            s"andThen flatMap JPredicate $w1 $t" !! j.justification(t) map {
              case w2 if w1.nonEmpty && w2.nonEmpty => w1 + "&" + w2
              case _ if w1.nonEmpty => w1
              case w2 => w2
            }
          case _ if p(t) => s"flatMap Predicate $w1 $t" !! Some(w1)
          case _ => None
        })
}

/**
  * The `JPredicate` companion object offers utility methods for constructing and manipulating `JPredicate` instances.
  *
  * It provides methods to create predicates with specific behaviors, such as always matching, never matching,
  * or conditional matching based on a function or condition.
  * The object relies on a logging mechanism (`flog`)
  * to help trace the logical flow and justification of predicate evaluations.
  */
object JPredicate {
  val flog = Flog[JPredicate[?]].disabled

  import flog._

  /**
    * Constructs a `JPredicate` from a provided function `f` that maps an input of type `T`
    * to an `Option[String]`.
    * The resulting predicate evaluates the input using `f`, and
    * the justification string is determined by the resulting `Option[String]`.
    *
    * @param f a function that takes an input of type `T` and returns an `Option[String]`.
    *          The `Option[String]` represents the justification for the predicate to succeed.
    * @tparam T the input type of the predicate.
    * @return a `JPredicate[T]` that applies the given function `f` to evaluate the justification.
    */
  def apply[T](f: T => Option[String], tag: String = ""): JPredicate[T] = (t: T) => s"JPredicate.apply(${showF(f, tag)}: $t" !! f(t)

  /**
    * Constructs a `JPredicate[T]` that evaluates a given condition on an input `T`
    * and provides a justification message if the condition is true.
    *
    * @param w a lazy-evaluated string representing the justification message when the predicate passes.
    * @param p a predicate function that takes an input of type `T`
    *          and returns a boolean indicating whether the condition is satisfied.
    * @tparam T the input type of the predicate.
    * @return a `JPredicate[T]` that encapsulates the provided condition and justification logic.
    */
  def when[T](w: => String)(p: T => Boolean): JPredicate[T] = apply(t => s"JPredicate.when($w)(${showF(p, w)} $t" !! Option.when(p(t))(w), "predicate on T")

  /**
    * Constructs a `JPredicate[T]` that always evaluates to `true` and provides a constant justification message.
    *
    * @param w the constant justification message as a `String`.
    * @tparam T the input type of the predicate.
    * @return a `JPredicate[T]` that always succeeds with the provided justification message.
    */
  def always[T](w: String): JPredicate[T] = (_: T) => s"JPredicate.always($w)" !! Some(w)

  /**
    * Constructs a `JPredicate[T]` that always evaluates to `false` and does not provide any justification.
    *
    * @tparam T the input type of the predicate.
    * @return a `JPredicate[T]` that never succeeds.
    */
  def never[T]: JPredicate[T] = (_: T) => None

  private def showF(f: ? => ?, tag: String) = f match {
    case NamedFunction(name, _) => name
    case _ => tag
  }
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
    * @return an IntPredicate that behaves the same as `this` but with a different name in the case of a positive match.
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
    * A case class that wraps a function from type `T` to `R`, associating it with a descriptive name.
    *
    * CONSIDER achieving this through an implicit class mechanism using the ^^ method.
    *
    * @tparam T The input type of the function.
    * @tparam R The output type of the function.
    * @param name A descriptive name for the function.
    * @param f    The function to be wrapped.
    *
    *             This class extends the `Function1` trait, allowing instances of `NamedFunction` to
    *             be used as a standard function from `T` to `R`. The main utility of this class is to
    *             provide a named representation of the function for enhanced readability and debugging.
    */
  case class NamedFunction[T, R](name: String, f: T => R) extends (T => R) {
    override def apply(t: T): R = f(t)

    override def toString: String = s"$name, T=>R"
  }

  /**
    * A companion object for constructing instances of `NamedFunction` with a specified name and function.
    *
    */
  object NamedFunction {
    /**
      * Creates an instance of `NamedFunction` by wrapping a given function with a specific name.
      *
      * @param name A descriptive name for the function.
      * @param f    The function to be wrapped, defined as a function from type `T` to `R`.
      * @tparam T The input type of the function.
      * @tparam R The output type of the function.
      * @return A `NamedFunction` instance that associates the provided name with the given function.
      */
    def apply[T, R](name: String, f: T => R): NamedFunction[T, R] = new NamedFunction(name, f)

    /**
      * Implicit class that enriches a plain function of type `T => R` with the ability to attach a descriptive name,
      * resulting in a `NamedFunction` instance.
      *
      * This can be useful for improving code readability, debugging, and logging, as it provides a way to associate
      * a human-readable identifier with the function's behavior or purpose.
      *
      * @tparam T The input type of the function.
      * @tparam R The output type of the function.
      * @param f The original function to be enriched.
      */
    implicit class RawFunction[T, R](f: T => R) {
      def ^^(name: String): NamedFunction[T, R] = NamedFunction(name, f)
    }

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

