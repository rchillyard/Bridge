/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import scala.util.control.NonFatal

/**
  * Case class to define an identity flow of X, which can be sampled or "tapped" by a X=>Unit function "tee".
  * The purpose of this case class is to facilitate side-effects which do not interrupt the structure of a functional expression.
  *
  * @param tee       the sampling function (X=>Unit) which operates as a side-effect (defaults to a no-op).
  * @param predicate a filter function (X=>Boolean) which causes tee to be invoked only when the X input value satisfies the predicate (defaults to always true).
  * @tparam X the input and output type of the apply function of the pipe.
  */
case class Pipe[X](tee: X => Unit = Pipe.noop _, predicate: X => Boolean = Pipe.always) extends (X => X) {
  self =>

  /**
    * The apply function of this Pipe.
    *
    * @param x the given value of x.
    * @return the same value of x but after invoking the side-effect defined by tee.
    */
  override def apply(x: X): X = {
    try if (predicate(x)) tee(x)
    catch {
      case NonFatal(e) => throw PipeException("in tee", e)
    }
    x
  }

  /**
    * Method to yield a new Pipe with the predicate inverted.
    *
    * @return a new Pipe[X] which will operate on all X values which fail predicate.
    */
  def not: Pipe[X] = new Pipe[X](tee, self.predicate.andThen(b => !b))

  /**
    * Method to yield a new Pipe where the predicate is conjoined with another predicate f.
    *
    * @param f a filter function (X=>Boolean) which causes tee to be invoked only when the X input value satisfies both predicate and f.
    * @return a new Pipe[X] which will operate on all X values which satisfy predicate and f.
    */
  def and(f: X => Boolean): Pipe[X] = new Pipe[X](tee, Pipe.and(self.predicate, f))

  /**
    * Method to yield a new Pipe where the predicate is disjoined with another predicate f.
    *
    * @param f a filter function (X=>Boolean) which causes tee to be invoked only when the X input value satisfies either predicate or f.
    * @return a new Pipe[X] which will operate on all X values which satisfy predicate or f.
    */
  def or(f: X => Boolean): Pipe[X] = new Pipe[X](tee, Pipe.or(self.predicate, f))
}

object Pipe {
  private def and[X](predicate: X => Boolean, f: X => Boolean): X => Boolean = x => predicate(x) && f(x)

  private def or[X](predicate: X => Boolean, f: X => Boolean): X => Boolean = x => predicate(x) || f(x)

  private def always[X]: X => Boolean = _ => true

  private def noop[X](x: X): Unit = ()
}

case class PipeException(msg: String, e: Throwable = null) extends Exception(s"Pipe exception: $msg", e)