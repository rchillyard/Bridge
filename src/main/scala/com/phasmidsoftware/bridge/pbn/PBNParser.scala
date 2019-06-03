/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.pbn

import scala.io.Source
import scala.language.postfixOps
import scala.util._
import scala.util.parsing.combinator.JavaTokenParsers


/**
	* RecapParser will parse a String as either an event, section, preamble, pair, traveler, play or result.
	*/
class PBNParser extends JavaTokenParsers {
	override def skipWhitespace: Boolean = false

	def pbn: Parser[PBN] = repsep(game, emptyLine) ^^ { gs => PBN(gs) }

	def game: Parser[Game] = repsep(tagPair, emptySpace) ^^ { ps => Game(ps map { case n ~ v ~ xs => n -> DetailedValue(v, xs) }) }

	def emptySpace: Parser[Any] = """\s*""".r <~ opt(endOfLine)

	def tagPair: Parser[Name ~ Value ~ Seq[String]] =
		openBracket ~> name ~ (atLeastOneWhiteSpace ~> quote ~> value <~ quote <~ closeBracket) ~ detail

	def name: Parser[Name] = tag ^^ { s => Name(s) }

	def value: Parser[Value] = date | deal | set | stringValue | failure("invalid value")

	def detail: Parser[Seq[String]] = repsep(detailR, emptySpace)

	private def detailR = """[^;\[]+""".r

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

	/**
		* parser for the (insigificant) end of a line, which may include a comment.
		*
		* @return a Parser[Any]
		*/
	def endOfLine: Parser[Any] = semi ~ """.*\n""".r

	/**
		* A String not containing either a quote or a semi-colon
		*/
	private val generalString = """[^";]*""".r

	private val semi = """;""".r

	private val emptyLine = """\s*\n""".r

	private def nDigits(n: Int) = s"""\\d{$n}""".r

	private val tag = """[A-Z][a-zA-Z0-9_]*""".r

	private val quote = """"""".r

	private val period = """\.""".r

	private val colon = """\:""".r

	private val openBracket = """\[""".r

	private val closeBracket = """\]""".r

	private val atLeastOneWhiteSpace = """\s+""".r
}


object PBNParser {
	def parsePBN(s: Source): Try[PBN] = if (s != null) {
		val p = new PBNParser
		// NOTE: in the following two lines we take care of the escape mechanism (lines starting with %),
		// NOTE: and also the block comment mechanism (sequences matching {...} which may include newlines)
		val lines = s.getLines() filterNot (_.startsWith("%"))
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
