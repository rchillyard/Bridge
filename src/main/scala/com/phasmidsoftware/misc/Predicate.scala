/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.misc

/**
  * Trait to describe the behavior of Predicate.
  * Predicate extends (T => Boolean) so is a pure function that implements the <code>apply(t)</code> method.
  *
  * TODO (6) is Predicate a monad? Why or why not?
  *
  * TODO (5) why isn't there a <code>def apply(t: T): Boolean</code> declaration here?
  *
  * @tparam T the underlying type, that's to say the type of the input to <code>this</code> Predicate.
  */
trait Predicate[T] extends (T => Boolean) {

  // TODO (5) What do you think is the purpose of the following construct (what problem does it solve)? [I don't think I've taught this but you should be able to figure it out.]
  self =>

  /**
    * Method to transform <code>this</code> Predicate[T] into a Predicate[U].
    *
    * TODO (8 points) why is this method called <code>lens</code> and not <code>map</code>?
    *
    * @param f a function which takes a U and returns a T.
    * @tparam U the underlying type of the result.
    * @return a Predicate[U].
    */
  def lens[U](f: U => T): Predicate[U] = (u: U) => self(f(u))

  /**
    * Method to reverse the sense of this Predicate[T].
    *
    * @return a Predicate[T] that returns true when <code>this</code> Predicate would return false; and false when <code>this</code> would return true.
    */
  def flip: Predicate[T] = (t: T) => !self(t)

  /**
    * Method to compose a Predicate[T] from <code>this</code> and <code>p</code>.
    *
    * @param p a Predicate[T]
    * @return a Predicate[T] which will yield true for a <code>t</code> value only when <code>this</code> and <code>p</code> yield true.
    *         NOTE that <code>p</code> will not be invoked if <code>this</code> yields false.
    */
  def andThen(p: Predicate[T]): Predicate[T] = (t: T) => self(t) && p(t)

  /**
    * Method to compose a Predicate[T] from <code>this</code> or <code>p</code>.
    *
    * TODO (Bonus: 5) can you rewrite the expression of this method using only <code>flip</code> and <code>andThen</code>? Hint: DeMorgan's rule.
    *
    * @param p a Predicate[T]
    * @return a Predicate[T] which will yield true for a <code>t</code> value when <code>this</code> or <code>p</code> yield true.
    *         NOTE that <code>p</code> will not be invoked if <code>this</code> yields true.
    */
  def orElse(p: Predicate[T]): Predicate[T] = (t: T) => self(t) || p(t)

  /**
    * Method to compose a Predicate[T] from <code>this</code> implies <code>p</code>.
    *
    * @param p a Predicate[T]
    * @return a Predicate[T] which will yield true for a <code>t</code> value when <code>this</code> implies <code>p</code> yields true.
    *         NOTE that <code>p</code> will not be invoked if <code>this</code> yields true.
    */
  def implies(p: Predicate[T]): Predicate[T] = (t: T) => if (self(t)) p(t) else true
}

import com.phasmidsoftware.misc.Predicate.maybeShow

/**
  * NOTE: strictly speaking, this does not need to be abstract.
  *
  * @param name        the name of the predicate.
  * @param f           the predicate function.
  * @param showMatches whether to show matches (default should be false).
  * @tparam T the underlying type.
  */
abstract class BasePredicate[T](name: String, f: T => Boolean, showMatches: Boolean = false) extends Predicate[T] {
  def apply(t: T): Boolean = maybeShow(f(t), s"$name matched for t=$t", showMatches)
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
  def apply(t: T): Boolean = maybeShow(f(r)(t), s"$name(r=$r) matched for t=$t", showMatches)
}

/**
  * Case class representing a concrete Predicate[Int].
  *
  * TODO (8) Define another case class StringPredicate that extends BasePredicate[String]. You do NOT need to define a <code>named</code> method.
  *
  * @param name the name of the predicate.
  * @param f    the predicate function.
  */
case class IntPredicate(name: String, f: Int => Boolean) extends BasePredicate[Int](name, f) {
  /**
    * Method to copy this Predicate but with a different name.
    *
    * @param w a String to be used as the new name.
    * @return a IntPredicate that behaves the same as <code>this</code> but with a different name in the case of a positive match.
    */
  def named(w: String): IntPredicate = copy(name = w)
}

/**
  * Companion object to the trait Predicate.
  *
  * TODO (7) what are the rules governing whether a Class/Trait and an Object are companions?
  */
object Predicate {

  def apply[T](partialFunction: PartialFunction[T, Boolean]): Predicate[T] = (v1: T) => partialFunction(v1)

  //  val weird = Predicate[String]{case "Hello" => true}

  // TODO (10) what does the expression "_ :| 2" mean? Where is ":|" defined? What is the name for the general mechanism in use here?
  val isEven: Predicate[Int] = IntPredicate("even", _ :| 2)

  // TODO (5) why did the author of this code not simply write "val isOdd: Predicate[Int] = IntPredicate("odd", x => !(x :| 2))"
  // TODO (Bonus: 3) as the code stands, what is the disadvantage of the actual definition (Hint: what happens when there's a match?)
  // TODO (Bonus (hard): 5) fix that problem while still retaining the code <code>isEven.flip</code>
  val isOdd: Predicate[Int] = isEven.flip

  val isPositive: Predicate[Int] = IntPredicate("pos", _ > 0)

  /**
    * Return the value of <code>b</code> and (as a side-effect), if it is true and if <code>show</code> is true, then print the <code>msg</code>.
    *
    * TODO (7) is this method pure? What does it mean by "as a side-effect?"
    *
    * TODO (8) why does the declaration of <code>msg</code> have a rocket symbol "=>". Give the name of the construct AND offer a reason why it should be used here.
    *
    * @param b    a Boolean.
    * @param msg  a String to be conditionally printed.
    * @param show if true (and if <code>b</code> is true) then print <code>msg</code>.
    * @return <code>b</code>.
    */
  def maybeShow(b: Boolean, msg: => String, show: Boolean): Boolean = {
    if (b && show) println(msg)
    b
  }

  /**
    * Implicit class to represent a compound number (i.e., not a prime).
    *
    * TODO (6) Explain (briefly) how an implicit class works.
    * TODO (6) What is the real purpose of this particular class (Hint: I want to know how it is used)?
    *
    * @param x the value of the number.
    */
  implicit class Compound(x: Int) {
    /**
      * TODO (5) explain what this method does.
      *
      * @param y an Int.
      * @return a Boolean.
      */
    def :|(y: Int): Boolean = x % y == 0
  }
}
