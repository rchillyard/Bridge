/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import org.slf4j.{Logger, LoggerFactory}


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
      * Method to raise an exception (as a side-effect) in the case that p(x) yields false AND isEnabledInvariants is true.
      *
      * @param p the predicate which, if false, will trigger an exception (if isEnabledInvariants is also true).
      * @return a Pipe[X] which is also the identity[X] function.
      */
    def invariant(p: X => Boolean): X = Pipe[X](raiseException(buildMessage(x, "Invariant proved false for {}")), p).not.and(_ => isEnabledInvariants)(x)

    private def raiseException(msg: String): X => Unit = x => throw SmartValueOpsException(buildMessage(x, msg))

    /**
      * Method to log a warning message (as a side-effect) in the case that p(x) yields false AND isEnabledInvariants is true.
      *
      * @param p      the predicate which, if false, will trigger an exception (if isEnabledInvariants is also true).
      * @param logger the logger to which any messages will be appended.
      * @param msg    the message to be logged (the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}).
      * @return a Pipe[X] which is also the identity[X] function.
      */
    def invariant(p: X => Boolean, logger: Logger, msg: String): X = Pipe[X](doWarn(logger, msg), p).not.and(_ => isEnabledInvariants)(x)

    /**
      * Method to print to the Console a message (as a side-effect) in the case that p(x) yields false AND isEnabledInvariants is true.
      *
      * @param p   the predicate which, if false, will trigger an exception (if isEnabledInvariants is also true).
      * @param msg the message to be logged (the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}).
      * @return a Pipe[X] which is also the identity[X] function.
      */
    def invariant(p: X => Boolean, msg: String): X = Pipe[X](doPrint(msg), p).not.and(_ => isEnabledInvariants)(x)

    /**
      * Method to print a message to the Console (as a side-effect), provided that the value of isEnabledConsole is set to true.
      * See the Scala documentation on Console to see how to redirect the output to something else.
      *
      * @param msg the message to be printed (the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}).
      * @return a Pipe[X] which is also the identity[X] function.
      */
    def console(msg: String): X = Pipe[X](SmartValueOps.doPrint(msg)).and(_ => isEnabledConsole)(x)

    /**
      * Method to log a message via debug.
      *
      * @param logger a suitable logger (defaults to SmartValueLogger).
      * @param msg    the message where the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}.
      * @return the value of x from this SmartValue.
      */
    def debug(msg: String, logger: Logger = smartValueLogger): X = Pipe[X](x => logger.debug(buildMessage(x, msg)))(x)

    /**
      * Method to log a message via info.
      *
      * @param logger a suitable logger (defaults to SmartValueLogger).
      * @param msg    the message where the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}.
      * @return the value of x from this SmartValue.
      */
    def info(msg: String, logger: Logger = smartValueLogger): X = Pipe[X](x => logger.info(buildMessage(x, msg)))(x)

    /**
      * Method to log a message via warn.
      *
      * @param logger a suitable logger (defaults to SmartValueLogger).
      * @param msg    the message where the value of X is either appended to this message or, if the message contains "{}" then it will be substituted for {}.
      * @return the value of x from this SmartValue.
      */
    def warn(msg: String, logger: Logger = smartValueLogger): X = Pipe[X](SmartValueOps.doWarn(logger, msg))(x)

  }

  private val smartValueLogger: Logger = LoggerFactory.getLogger(SmartValueOps.getClass)

  //noinspection ScalaStyle
  private def doPrint[X](msg: String): X => Unit = x => Console.println(buildMessage(x, msg))

  private def doWarn[X](logger: Logger, msg: String): X => Unit = x => logger.warn(buildMessage(x, msg))

  private def buildMessage[X](x: X, msg: String): String = {
    val s = if (msg contains brackets) msg else msg + ": " + brackets
    s.replace(brackets, x.toString)
  }

  /**
    * Method to turn invariants on or off.
    *
    * @param b the boolean value to be assigned to isEnabledInvariants.
    */
  def setEnabledInvariants(b: Boolean): Unit = {
    isEnabledInvariants = b
  }

  /**
    * This is the boolean which controls whether invariant testing is enabled.
    */
  private var isEnabledInvariants: Boolean = true

  /**
    * Method to turn the console on or off.
    *
    * @param b the boolean value to be assigned to isEnabledConsole.
    */
  def setEnabledConsole(b: Boolean): Unit = {
    isEnabledConsole = b
  }

  /**
    * This is the boolean which controls whether invariant testing is enabled.
    */
  private var isEnabledConsole: Boolean = true

  /**
    * This is a pattern which, if found in a message, will be substituted for.
    */
  private val brackets: String = "{}"

}



case class SmartValueOpsException(msg: String) extends Exception(s"SmartValueOps exception: $msg")
