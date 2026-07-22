/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.gambit.util.{LazyLogger, Output}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.output.Using

import java.io.{FileWriter, PrintWriter}
import scala.annotation.unused
import scala.io.Source
import scala.language.postfixOps
import scala.util.*

/**
  * Created by scalaprof on 4/12/16.
  *
  */
object Score {
  private val logger = LazyLogger(getClass)

  // ... rest unchanged
  private val outputFile = "output.csv"
  private lazy val fileWriter = new FileWriter(outputFile)
  private lazy val printWriter = new PrintWriter(System.out)
  private lazy val tabbedOutput: Output = Output(fileWriter)
  @unused
  private lazy val untabbedOutput: Output = Output.untabbedWriter(printWriter, 6)
  // TODO set this to use untabbedOutput if you want all tabs turned into spaces.
  private lazy val defaultOutput: Output = tabbedOutput


  def main(args: Array[String]): Unit =
    if (doMain(args)(tabbedOutput)) println(s"Successful output to $outputFile")

  // TESTME
  def doMain(args: Array[String])(output: Output): Boolean = {
    if (args.length > 0) {
      val delimiter: String = (args lift 1).getOrElse("")
      doScoreFromName(isResource = false, args.head, delimiter, output) match {
        case Success(o) =>
          o.close()
          true
        case Failure(x) =>
          logger.error(s"Score ${args.mkString}: parsing failed due to an exception:")
          logger.error(x.getLocalizedMessage.translateEscapes())
          false
      }
    } else {
      System.err.println("Syntax: Score filename")
      false
    }
  }

  def doScoreFromName(isResource: Boolean, name: String, delimiter: String, output: Output = defaultOutput) = Using(
    if (isResource) Source.fromResource(name) else Source.fromFile(name)
  ) {
    logger.info(s"Opened source from $name")
    s => doScore(s, delimiter, output)
  }

  // TODO use the methods in Result
  def doScoreResource(resource: String, output: Output = defaultOutput): Try[Output] =
    Option(getClass.getResourceAsStream(resource)) match {
      case Some(s) => doScore(Source.fromInputStream(s), "", output)
      case None => Failure(ScoreException(s"doScoreResource: cannot open resource: $resource"))
    }

  def doScoreFromFile(filename: String, output: Output = defaultOutput): Try[Output] = doScore(Source.fromFile(filename), "", output)

  // TESTME

  /**
    * Method to do the scoring, given a source, a delimiter, and an Output.
    *
    * @param source a Source.
    * @param delimiter an additional delimiter(s) above and beyond space and tab.
    * @param output the output filename.
    * @return Output.
    */
  def doScore(source: Source, delimiter: String, output: Output = defaultOutput) = {

    implicit val separator: Output = Output.empty.insertBreak

    def eventResults(e: Event, p: Preamble, rs: Seq[Result], boards: Int): Output = {
      val z = for {
        r <- rs
        _ = r.checksum(boards)
      } yield r.getResults(p.getNames)
      (Output(s"Section ${p.identifier}") ++ z :+
        "=====================================================\n" :+
        "=====================================================\n") ++
        e.output(Output.empty)
    }

    val ey = RecapParser.readEvent(source, delimiter)

    for (e <- ey; x = e.score) yield (output :+ (x.title + "  " + e.hashCode().toHexString)).insertBreak ++ (for ((p, rs) <- x.createResults) yield eventResults(x, p, rs, x.boards))
  }

  /**
    * Method to transform a Rational (r) into a percentage.
    *
    * CONSIDER This method belongs in Rational.
    *
    * @param r    a Rational.
    * @param base the value corresponding to 100%.
    * @return r converted to a percentage of base.
    */
  def asPercent(r: Rational, base: Int): Rational = r * 100 / base
}

case class ScoreException(str: String) extends Exception(str)
