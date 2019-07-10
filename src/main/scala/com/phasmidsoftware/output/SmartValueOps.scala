/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import java.io.{PrintStream, PrintWriter, Writer}

import org.slf4j.Logger

object SmartValueOps {

  private var isEnabled: Boolean = true

  def setEnabled(b: Boolean): Unit = { isEnabled = b }

  /**
    * This is pattern which, if found in a message, will be substituted for.
    */
  private val brackets: String = "{}"

  implicit class SmartValue[X](x: X) {

    def invariant(p: X=>Boolean): X = Pipe[X](and(not(p),isEnabled), raiseException(buildMessage(x, "Invariant proved false for {}")))(x)

    def invariant(p: X=>Boolean, logger: Logger, msg: String): X = Pipe[X](and(not(p),isEnabled), doLog(logger, msg))(x)

    def invariant(p: X=>Boolean, msg: String): X = Pipe[X](and(not(p),isEnabled), doPrint(msg))(x)

    private def raiseException(msg: String): X => Unit = x => throw InvariantException(buildMessage(x, msg))

    private def doLog(logger: Logger, msg: String): X => Unit = x => logger.warn(buildMessage(x, msg), x)

    //noinspection ScalaStyle
    private def doPrint(msg: String): X => Unit = x => Console.println(buildMessage(x, msg))

    private def doOutput(output: Output, msg: String): X => Unit = x => output :+ msg

    private def buildMessage(x: X, msg: String): String = {
      val s = if (msg contains brackets) msg else msg + ": " + brackets
      s.replace(brackets, x.toString)
    }

    private def not(p: X => Boolean): X => Boolean = x => !p(x)

    private def and(p: X => Boolean, b: Boolean): X => Boolean = x => b && p(x)
  }


}


