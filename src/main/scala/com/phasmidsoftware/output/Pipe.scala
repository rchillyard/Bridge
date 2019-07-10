/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

/**
  * Case class to define an identity flow of X, which can be sampled or "tapped" by a function "tee".
  *
  * @param tee the sampling function which operates as a side-effect.
  * @tparam X the input and output type of the apply function of the pipe.
  */
case class Pipe[X](tee: X=>Unit) extends (X => X) {
  override def apply(x: X): X = { tee(x); x}
}

object Pipe {
  /**
    * Create a conditional Pipe based on the predicate p and the tee function.
    * @param p a predicate on X.
    * @param tee the tee function.
    * @tparam X the input and output type of the apply function of the pipe.
    * @return a Pipe[X].
    */
  def apply[X](p: X => Boolean, tee: X => Unit): Pipe[X] = Pipe(x => if (p(x)) tee(x))
}