/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import org.slf4j.Logger

/**
  * Object SmartValueOps
  *
  * This object defines an implicit class SmartValue which can be used to wrap any value and provide some cross-cutting methods,
  * for example, logging, invariant testing (like assertion), etc.
  *
  *
  */
object SmartValueOps {

  /**
    * Implicit class SmartValue.
    * If you try to invoke one of SmartValue's methods then, provided that the class is in scope, you will get a SmartValue implicitly.
    *
    * @param x the value to be wrapped by SmartValue.
    * @tparam X the underlying type of the SmartValue.
    */
  implicit class SmartValue[X](x: X) {

    /**
      * Method to raise an exception (as a side-effect) in the case that p(x) yields false AND invariantsEnabled is true.
      *
      * @param p the predicate which, if false, will trigger an exception (if invariantsEnabled is also true).
      * @return a Pipe[X] which is also the identity[X] function.
      */
    def invariant(p: X => Boolean): X = Pipe[X](raiseException(buildMessage(x, "Invariant proved false for {}")), p).not.and(_ => invariantsEnabled)(x)

    private def raiseException(msg: String): X => Unit = x => throw SmartValueOpsException(buildMessage(x, msg))

    /**
      * Method to log a warning message (as a side-effect) in the case that p(x) yields false AND invariantsEnabled is true.
      *
      * @param p      the predicate which, if false, will trigger an exception (if invariantsEnabled is also true).
      * @param logger the logger to which any messages will be appended.
      * @param msg    the message to be logged (the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}).
      * @return a Pipe[X] which is also the identity[X] function.
      */
    def invariant(p: X => Boolean, logger: Logger, msg: String): X = Pipe[X](doLog(logger, msg), p).not.and(_ => invariantsEnabled)(x)

    private def doLog(logger: Logger, msg: String): X => Unit = x => logger.warn(buildMessage(x, msg))

    private def buildMessage(x: X, msg: String): String = {
      val s = if (msg contains brackets) msg else msg + ": " + brackets
      s.replace(brackets, x.toString)
    }

    /**
      * Method to print to the Console a message (as a side-effect) in the case that p(x) yields false AND invariantsEnabled is true.
      *
      * @param p   the predicate which, if false, will trigger an exception (if invariantsEnabled is also true).
      * @param msg the message to be logged (the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}).
      * @return a Pipe[X] which is also the identity[X] function.
      */
    def invariant(p: X => Boolean, msg: String): X = Pipe[X](doPrint(msg), p).not.and(_ => invariantsEnabled)(x)

    //noinspection ScalaStyle
    private def doPrint(msg: String): X => Unit = x => Console.println(buildMessage(x, msg))
  }

  /**
    * Method to turn invariants on or off.
    *
    * @param b the boolean value to be assigned to invariantsEnabled.
    */
  def setInvariantsEnabled(b: Boolean): Unit = {
    invariantsEnabled = b
  }

  /**
    * This is a pattern which, if found in a message, will be substituted for.
    */
  private val brackets: String = "{}"

  /**
    * This is the boolean which controls whether invariant testing is enabled.
    */
  private var invariantsEnabled: Boolean = true

}

case class SmartValueOpsException(msg: String) extends Exception(s"SmartValueOps exception: $msg")
