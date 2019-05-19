package com.phasmidsoftware.output

import org.slf4j.LoggerFactory

import scala.reflect.ClassTag

/**
	* Simple functional logging utility.
	*
	* Here are the steps you need to follow to enable logging:
	*
	* <ol>
	* <li>In your code, somewhere in scope (implicits rules), import Flog._</li>
	* <li>Create a String which will form the log message, follow it with "!!" and follow that with the expression you want to log.</li>
	* <li>If you wish to override the logging function, then declare something like the following:
	* <code>implicit def logFunc(w: String): LogFunction = LogFunction(println)</code>
	* In this case, you will need to explicitly construct an instance of FlogIt from your message string, such as:
	* <code>FlogIt(getString)(logFunc)</code>
	* Follow this with !! and the expression, as usual.
	* </li>
	* <li>If you wish to override the default LogFunction, for example to log according to a particular class,
	* then you can also set it (it is declared as a var):
	* <code>Flog.loggingFunction = Flog.getLogger[FlogSpec]</code>
	* </li>
	* </ol>
	*
	* There are several ways to turn logging off (temporarily or permanently) once you've added the log expressions:
	* (1A) replace the !! method with the |! method for each expression you wish to silence;
	* (1B) define an implicit LogFunction which does nothing (but this will involve explicitly constructing a FlogIt, as described above);
	* (2) set Flog.enabled = false in your code (silences all logging everywhere);
	* (3) remove the !! expressions.
	*/
object Flog {

	/**
		* Implicit class to implement logging.
		*
		* If you are using the default logging function (Flog.loggingFunction), then you can instantiate and utilize
		* a new instance of Flog simply by applying the "!!" operator to a String.
		* However, if you are using a locally defined value of the logging function, you will have to instantiate a FlogIt
		* explicitly (see FlogSpec for examples).
		*
		* @param message the message itself which will be evaluated only if enabled is actually turned on.
		* @param logFunc the logging function to be used for this log message (defaults to Flog.loggingFunction).
		*/
	implicit class FlogIt(message: => String)(implicit logFunc: LogFunction = Flog.loggingFunction) {
		/**
			* Method to generate a log entry.
			* Logging is performed as a side effect.
			*
			* @param x the value to be logged.
			* @tparam X the type of x.
			* @return the value of x.
			*/
		def !![X: ClassTag](x: => X): X = Flog.log(logFunc, message)(x)

		/**
			* Method to simply return the value of x without any logging.
			*
			* @param x the value.
			* @tparam X the type of x.
			* @return the value of x.
			*/
		def |![X](x: => X): X = x
	}

	/**
		* The master switch.
		* Logging only occurs if this variable is true.
		*/
	var enabled = true

	/**
		* The default logging function which logs to the info method of the logger for the FlogIt class.
		*/
	implicit var loggingFunction: LogFunction = getLogger[FlogIt]

	def getLogger[T: ClassTag]: LogFunction = LogFunction(LoggerFactory.getLogger(implicitly[ClassTag[T]].runtimeClass).info)

	def log[X: ClassTag](logFunc: LogFunction, message: => String)(x: => X): X = {
		lazy val xx = x
		if (enabled) {
			val msg = s"log: $message" + (if (implicitly[ClassTag[X]].runtimeClass == classOf[Unit]) "" else
				s": $xx")
			logFunc(msg)
		}
		xx
	}
}

case class LogFunction(f: String => Any) {
	def apply(w: String): Unit = f(w)
}