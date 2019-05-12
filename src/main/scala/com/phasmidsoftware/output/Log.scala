package com.phasmidsoftware.output

import scala.reflect.ClassTag

case class Log(message: String) {
	def ![X: ClassTag](x: => X): X = Log.log(message)(x)

	//noinspection UnitMethodIsParameterless
	def ! : Unit = Log.log(message)(())
}

object Log {
	def log[X: ClassTag](message: String)(x: => X): X = {
		lazy val xx = x
		val msg = s"log: $message" + (if (implicitly[ClassTag[X]].runtimeClass == classOf[Unit]) "" else
			s": $xx")
		System.err.println(msg)
		xx
	}
}