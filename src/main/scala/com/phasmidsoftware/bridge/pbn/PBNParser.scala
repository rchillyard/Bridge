/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.pbn

import scala.io.Source
import scala.language.postfixOps
import scala.util._
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * RecapParser will parse a String as either an event, section, preamble, pair, traveler, play or result.
  */
class PBNParser extends JavaTokenParsers {
  override def skipWhitespace: Boolean = false

  /**
    * Method to parse a PBN as a sequence of game separated by the gameTerminator.
    *
    * @return a Parser[PBN]
    */
  def pbn: Parser[PBN] = repsep(game, """\|\|\|\|\|\|\|\|\|\|\n""".r) ^^ { gs => PBN(gs) }

  // TODO what's this?
  //	def pbn: Parser[PBN] = repsep(game, PBNParser.gameTerminatorR) ^^ ( gs => PBN(gs) )

  def game: Parser[Game] = repsep(tagPair, emptySpace) ^^ { ps =>
    Game(ps map { case n ~ v ~ xs => n -> DetailedValue.trim(v, xs) })
  }

  def tagPair: Parser[Name ~ Value ~ Seq[String]] =
    openBracket ~> name ~ (atLeastOneWhiteSpace ~> quote ~> value <~ quote <~ closeBracket) ~ detail

  def name: Parser[Name] = tag ^^ { s => Name(s) }

  def value: Parser[Value] = date | deal | set | stringValue | failure("invalid value")

  def detail: Parser[Seq[String]] = repsep(detailR, emptySpace) ^^ { ws =>
    for (w <- ws; x <- w.split("\n")) yield x
  }

  def date: Parser[DateValue] = {
    val yearDigits = 4
    val monthDayDigits = 2
    (digits(yearDigits) <~ period) ~ (digits(monthDayDigits) <~ period) ~ digits(monthDayDigits) ^^ { case y ~ m ~ d => DateValue(y, m, d) }
  }

  def deal: Parser[DealValue] = (playerGlyphs <~ colon) ~ repsep(hand, atLeastOneWhiteSpace) ^^ { case d ~ hs => DealValue(d, hs) }

  def playerGlyphs: Parser[String] = """[NESW]""".r

  def hand: Parser[Seq[String]] = repsep(holding, period)

  def holding: Parser[String] = """[AKQJT2-9]*""".r

  def set: Parser[Value] = repsep(generalString, semi) ^^ {
    case Nil => StringValue("")
    case h :: Nil => StringValue(h)
    case ws => SetValue(ws)
  }

  def stringValue: Parser[StringValue] = generalString ^^ (s => StringValue(s))

  def digits(n: Int): Parser[Int] = nDigits(n) ^^ (x => x.toInt)

  def emptySpace: Parser[Any] = """\s*""".r <~ opt(endOfLine)

  /**
    * parser for the (insignificant) end of a line, which may include a comment.
    *
    * @return a Parser[Any]
    */
  def endOfLine: Parser[Any] = semi ~ """.*\n""".r

  /**
    * A String containing neither a quote nor a semi-colon.
    */
  private val generalString = """[^";]*""".r

  /**
    * NOTE: the vertical bar should be a reference to the unusedCharacter
    */
  private val detailR = """[^;|\[]+""".r

  private val semi = """;""".r

  //	private val emptyLine = """\s*\n""".r

  private def nDigits(n: Int) = s"""\\d{$n}""".r

  private val tag = """[A-Z][a-zA-Z0-9_]*""".r

  private val quote = """"""".r

  private val period = """\.""".r

  private val colon = """:""".r

  private val openBracket = """\[""".r

  private val closeBracket = """\]""".r

  private val atLeastOneWhiteSpace = """\s+""".r
}


object PBNParser {
  /**
    * Define a character not used by the PBN notation that we can use
    * to designate the end of a game.
    * The reason we need to do this is because the PBN definition is very vague about this
    * and allows multiple empty lines after a the detail following a tag.
    * Basically, the PBN definition was developed by somebody who had no clue!
    */
  private val unusedChar = """|"""

  private val gameTerminatorLength = 10

  val gameTerminator: String = unusedChar * gameTerminatorLength

  val gameTerminatorR: Regex = (gameTerminator + "\n").r

  /**
    * Parse a PBN file and return a Try of PBN.
    * NOTE: the PBN spec (https://www.tistis.nl/pbn/) is unbelievably bad!
    * This method is mostly concerned with pre-processing the source lines so that they can be parsed
    * by a real parser.
    * In particular, we filter out lines beginning with "%" (the so-called escape sequences);
    * Then we filter out blocks of code delimited by {} (these are block comments);
    * Then we replace pairs of empty lines with one single line containing ten vertical bars.
    *
    * @param s the source from which we will parse the PBN.
    * @return a Try[PBN]
    */
  def parsePBN(s: Source): Try[PBN] = if (s != null) {
    val p = new PBNParser
    // NOTE: would prefer to construct the terminator string ("||||||||||") from the unusedChar.
    val lines = s.getLines().filterNot(_.startsWith("%")).
      sliding(2, 1).
      filterNot(ws => ws.head.isEmpty && ws.last.isEmpty).
      map(_.head).
      map(s => if (s.isEmpty) "||||||||||" else s)
    val string = lines.mkString("\n").replaceAll("""\{[\n\S\s]*\}""", "")
    try p.parseAll(p.pbn, string) match {
      case p.Success(x: PBN, _) => Success(x)
      case f@p.Failure(_, _) => scala.util.Failure(new Exception(s"PBN parse failure: $f"))
      case p.Error(f, x) => scala.util.Failure(new Exception(s"PBN parse error: $f at $x"))
    }
    catch {
      case exception: Exception => Failure(exception)
    }
  } else Failure(new Exception("source is null"))
}
