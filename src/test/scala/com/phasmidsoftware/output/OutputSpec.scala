/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import com.phasmidsoftware.bridge.director.Card
import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.util.{Output, OutputException}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.Writer
import scala.util.Try

/**
  * This adds one test to the OutputSpec in the DecisionTree project.
  */
class OutputSpec extends AnyFlatSpec with should.Matchers {

  behavior of "BufferedCharSequenceOutput"

  it should "properly process complex expression" in {
    val writer = MockWriter()
    val output: Output = Output(writer)

    def getResultsForDirection(preamble: (String, Option[String], Seq[(Int, String, String)]), r: (Boolean, Int, Map[Int, (Rational, Int)]), top: Int): Output = {
      def resultDetails(s: (Int, (Rational, Int))): Output = Output(s"${s._1} : ${Card.mpsAsString(s._2._1, top)} : ${Card(s._2._1, s._2._2, 0).toStringPercent} : Tweedledum & Tweedledee").insertBreak()

      Output.foldLeft(r._3.toSeq.sortBy(_._2._1).reverse)()(_ ++ resultDetails(_))
    }

    def getResults(k: (String, Option[String], Seq[(Int, String, String)]), r: (Boolean, Int, Map[Int, (Rational, Int)])): Output = Output(s"Results for direction: ${if (r._1) "N/S" else "E/W"}").insertBreak ++ getResultsForDirection(k, r, r._2)

    def eventResults(e: (String, Seq[String]), k: (String, Option[String], Seq[(Int, String, String)]), rs: Seq[(Boolean, Int, Map[Int, (Rational, Int)])]): Output = {
      val z = for (r <- rs) yield getResults(k, r)
      (Output(s"${e._1}\nSection ${k._1}").insertBreak ++ z :+
        "=====================================================\n" :+
        "=====================================================\n") ++
        Output(e._1)
    }

    val ey = Try(("test", Seq("1", "2")))

    val zy: Try[Output] = for (e <- ey) yield {
      val results = for ((k, rs) <- Seq(("test", None, Seq((1, "x", "y"))) -> Seq((true, 2, Map[Int, (Rational, Int)]())))) yield eventResults(e, k, rs)
      (output :+ "XXX").insertBreak ++ results
    }

    zy foreach {
      _.close()
    }
    writer.spilled shouldBe 163
    writer.spillway shouldBe "XXX\n test\nSection test\n Results for direction: N/S\n=====================================================\n=====================================================\ntest"
  }
}

case class MockWriter(n: Int = 4096, var isOpen: Boolean = true) extends Writer {
  var length = 0
  var spilled = 0
  val chars: Array[Char] = new Array[Char](n)
  var spillway = ""

  def content: String = chars.mkString.substring(0, length)

  override def toString: String = s"""MockWriter: isOpen=$isOpen, length=$length, spilled=$spilled and content="$content""""

  def spill(len: Int): String = {
    val toSpill = math.min(length, length + len - n)
    if (toSpill > 0) {
      val result = chars.take(toSpill).mkString("")
      if (toSpill + length <= n)
        Array.copy(chars, toSpill, chars, 0, length)
      else
        throw OutputException(s"logic error: buffer too small: $n but needs to be at least ${toSpill + length}")
      spilled += toSpill
      length -= toSpill
      result
    }
    else
      ""
  }

  def write(cbuf: Array[Char], off: Int, len: Int): Unit =
    if (isOpen) {
      spillway = spill(len)
      if (len + length <= n) {
        Array.copy(cbuf, off, chars, length, len)
        length += len
      }
      else throw OutputException(s"MockWriter: buffer too small: $n but should be ${len + length}")
    }
    else throw new Exception(s"MockWriter is closed")

  def flush(): Unit = {
    spillway = spill(n)
  }

  def close(): Unit = {
    flush()
    isOpen = false
  }

  def total: Int = length + spilled
}