/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import org.slf4j.{Logger, LoggerFactory, Marker}

trait LazyLogger[L <: Logger] {
  def lazyTrace(l: L)(msg: => String): Unit = if (l.isTraceEnabled) l.trace(msg)
  def lazyDebug(l: L)(msg: => String): Unit = if (l.isDebugEnabled) l.debug(msg)
  def lazyInfo(l: L)(msg: => String): Unit = if (l.isInfoEnabled) l.info(msg)
}

object LazyLogger {
  implicit object LazyLoggerSlf4j extends LazyLogger[Logger]
}

///**
//  * This logger uses non-strict (i.e. lazy) parameters for messages, formats and arguments.
//  * Only parameters for logging levels trace, debug and info are evaluated lazily.
//  * The message strings are evaluated only if the appropriate level of debugging is turned on.
//  * All methods delegate to the underlying logger.
//  * This logger does not support the signatures involving Marker.
//  *
//  * @param logger
//  */
//case class SmartLogger(logger: Logger) {
//  lazy val getName: String = logger.getName
//
//  lazy val isTraceEnabled: Boolean = logger.isTraceEnabled
//
//  def trace(msg: => String): Unit = if (isTraceEnabled) logger.trace(msg)
//
//  def trace(format: => String, arg: => Any): Unit = if (isTraceEnabled)logger.trace(format, arg)
//
//  def trace(format: => String, arg1: => Any, arg2: => Any): Unit = if (isTraceEnabled)logger.trace(format, arg1, arg2)
//
//  def trace(format: => String, arguments: Any*): Unit = if (isTraceEnabled)logger.trace(format, arguments)
//
//  def trace(msg: => String, t: Throwable): Unit = if (isTraceEnabled) logger.trace(msg, t)
//
//  lazy val isDebugEnabled: Boolean = logger.isDebugEnabled
//
//  def debug(msg: => String): Unit = if (isDebugEnabled) logger.debug(msg)
//
//  def debug(format: => String, arg: => Any): Unit = if (isDebugEnabled) logger.debug(format, arg)
//
//  def debug(format: => String, arg1: => Any, arg2: => Any): Unit = if (isDebugEnabled) logger.debug(format, arg1, arg2)
//
//  def debug(format: => String, arguments: Any*): Unit = if (isDebugEnabled) logger.debug(format, arguments)
//
//  def debug(msg: => String, t: Throwable): Unit = if (isDebugEnabled) logger.debug(msg, t)
//
//  lazy val isInfoEnabled: Boolean = logger.isInfoEnabled
//
//  def info(msg: => String): Unit = if (isInfoEnabled) logger.info(msg)
//
//  def info(format: => String, arg: => Any): Unit = if (isInfoEnabled) logger.info(format, arg)
//
//  def info(format: => String, arg1: => Any, arg2: => Any): Unit = if (isInfoEnabled) logger.info(format, arg1, arg2)
//
//  def info(format: => String, arguments: Any*): Unit = if (isInfoEnabled) logger.info(format, arguments)
//
//  def info(msg: => String, t: Throwable): Unit = if (isInfoEnabled) logger.info(msg, t)
//
//  lazy val isWarnEnabled: Boolean = logger.isWarnEnabled
//
//  def warn(msg: String): Unit = logger.warn(msg)
//
//  def warn(format: String, arg: Any): Unit = logger.warn(format, arg)
//
//  def warn(format: String, arguments: Any*): Unit = logger.warn(format, arguments)
//
//  def warn(format: String, arg1: Any, arg2: Any): Unit = logger.warn(format, arg1, arg2)
//
//  def warn(msg: String, t: Throwable): Unit = logger.warn(msg, t)
//
//  def isErrorEnabled: Boolean = logger.isErrorEnabled
//
//  def error(msg: String): Unit = logger.error(msg)
//
//  def error(format: String, arg: Any): Unit = logger.error(format, arg)
//
//  def error(format: String, arg1: Any, arg2: Any): Unit = logger.error(format, arg1, arg2)
//
//  def error(format: String, arguments: Any*): Unit = logger.error(format, arguments)
//
//  def error(msg: String, t: Throwable): Unit = logger.error(msg, t)
//}

//object SmartLogger {
//  def apply(clazz: Class[_]): SmartLogger = apply(LoggerFactory.getLogger(clazz))
//}