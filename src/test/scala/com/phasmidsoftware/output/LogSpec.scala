package com.phasmidsoftware.output

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class LogSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

	var evaluated = false

	override def beforeEach(): Unit = {
		evaluated = false
	}

	override def afterEach() {
		Log.logging = true // we need to put the (singleton) value of logging back the way it was.
	}

	behavior of "Log"

	it should "$bang 1" in {
		def getString: String = {
			evaluated = true; "Hello"
		}

		val sb = new StringBuilder

		def logFunc(w: String): Unit = sb.append(w)

		Log("test", logFunc)(getString) ! 1
		evaluated shouldBe true
		sb.toString shouldBe "log: test: Hello: 1"
	}

	it should "$bang 2" in {
		def getString: String = {
			evaluated = true; "Hello"
		}

		val sb = new StringBuilder

		def logFunc(w: String): Unit = sb.append(w)
		//noinspection NameBooleanParameters
		Log("test", logFunc)(getString, false) ! 1
		evaluated shouldBe false
		sb.toString shouldBe ""
	}

	it should "$bang 3" in {
		def getString: String = {
			evaluated = true; "Hello"
		}

		val sb = new StringBuilder

		def logFunc(w: String): Unit = sb.append(w)

		Log.logging = false
		Log("test", logFunc)(getString) ! 1
		evaluated shouldBe false
		sb.toString shouldBe ""
	}

	/**
		* In this test, we should see logging output according to the default value of Log.loggingFunction
		*/
	it should "$bang 4" in {
		def getString: String = {
			evaluated = true; "Hello"
		}

		Log("test")(getString) ! 1
		evaluated shouldBe true
	}

	it should "$bar" in {
		def getString: String = {
			evaluated = true; "Hello"
		}

		val sb = new StringBuilder

		def logFunc(w: String): Unit = sb.append(w)

		Log("test", logFunc)(getString) | 1
		evaluated shouldBe false
		sb.toString shouldBe ""
	}

}
