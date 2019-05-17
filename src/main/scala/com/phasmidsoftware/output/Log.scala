package com.phasmidsoftware.output

import scala.reflect.ClassTag

/**
	* Simple functional logging utility.
	*
	* There are several ways to turn logging off (temporarily or permanently) once you've added the log expressions:
	* (1A) add a false argument to the second parameter set of each Log constructor that you wish to silence;
	* (1B) replace the ! method with the | method for each expression you wish to silence;
	* (2) set Log.logging = false in your code (silences all logging);
	* (3) remove the Log expressions. The simplest way to do this is to to use the following regex in the editor:
	* Log\([^\)]*\)\s*\([^\)]*\)!\s*
	*
	* @param prefix  the prefix to be used for the resulting log message.
	* @param logFunc the logging function to be used for this log message (defaults to Log.loggingFunction).
	* @param message the message itself which will be evaluated only if logging is actually turned on.
	* @param log     a Boolean which defaults to true but can be set to false to temporarily silence this Log message.
	*/
case class Log(prefix: String, logFunc: String => Unit = Log.loggingFunction)(message: => String, log: Boolean = true) {

	/**
		* Method to generate a log entry.
		* Logging is performed as a side effect.
		*
		* @param x the value to be logged.
		* @tparam X the type of x.
		* @return the value of x.
		*/
	def ![X: ClassTag](x: => X): X = if (log) Log.log(logFunc, prefix + ": " + message)(x) else x

	/**
		* Method to simply return the value of x without any logging.
		*
		* @param x the value.
		* @tparam X the type of x.
		* @return the value of x.
		*/
	def |[X: ClassTag](x: => X): X = x
}

object Log {
	var logging = true

	val loggingFunction: String => Unit = System.err.println

	def log[X: ClassTag](logFunc: String => Unit, message: => String)(x: => X): X = {
		lazy val xx = x
		if (logging) {
			val msg = s"log: $message" + (if (implicitly[ClassTag[X]].runtimeClass == classOf[Unit]) "" else
				s": $xx")
			logFunc(msg)
		}
		xx
	}
}