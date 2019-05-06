package com.phasmidsoftware.bridge.director

import scala.io.Source
import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, _}


/**
	* RecapParser will parse a String as either an event, section, preamble, pair, traveler, play or result.
	*/
class RecapParser extends JavaTokenParsers {
	override def skipWhitespace: Boolean = false

	// XXX event parser yields an Event and is a title followed by a list of sections
	def event: Parser[Event] = (title <~ endOfLine) ~ rep(section) <~ opt(eoi) ^^ { case p ~ ss => Event(p, ss) }

	// XXX section parser yields a Section and is a preamble followed by a list of travelers
	def section: Parser[Section] = preamble ~ travelers ~ pickups ^^ { case p ~ ts ~ ps => Section(p, ts, ps) }

	// XXX (section) preamble parser yields a Preamble and is one or two letters, endOfLine, followed by a list of pair results, each on its own line.
	def preamble: Parser[Preamble] = (sectionIdentifier ~ opt(spacer ~> modifier) <~ endOfLine) ~ pairs ^^ { case t ~ wo ~ ps => Preamble(t, wo, ps) }

	// XXX a modifier consisting of at least one capital letter
	def modifier: Parser[String] = """[A-Z]+""".r

	// XXX list of pairs, each terminated by a endOfLine
	def pairs: Parser[List[Pair]] = rep(pair <~ endOfLine)

	// XXX pair parser yields a Players object and is a number followed by "N" or "E" followed by two full names, each terminated by a period
	def pair: Parser[Pair] = (wholeNumber <~ spacer) ~ ("E" | "N") ~ playerPlayer ^^ { case n ~ d ~ p => Pair(n.toInt, d, p._1 -> p._2) }

	// XXX pair parser yields a tuple of Player objects and is a string, possibly including space characters but not including & or endOfLine.
	def playerPlayer: Parser[Player ~ Player] = (player <~"""&""") ~ player

	// XXX player parser yields a Player object and is at least one character that is neither & nor a newline char
	def player: Parser[Player] = """\s*\w[^\r\n&]*""".r ^^ (s => Player(s.trim))

	// XXX travelers, each terminated by a endOfLine
	def travelers: Parser[List[Traveler]] = rep(traveler)

	// XXX traveler parser yields a Traveler object and must start with a "T" and end with a blank line. In between is a list of Play objects
	def traveler: Parser[Traveler] = opt(spacer) ~> "T" ~> spacer ~> (wholeNumber <~ endOfLine) ~ plays <~ (endOfLine | eoi) ^^ { case b ~ ps => Traveler(Try(b.toInt), ps) }

	// XXX pickups, each terminated by a endOfLine
	def pickups: Parser[List[Pickup]] = rep(pickup)

	// XXX pickup parser yields a Pickup object and must start with a "P" and end with a blank line. In between is a list of Play objects
	def pickup: Parser[Pickup] = opt(spacer) ~> "P" ~> spacer ~> wholeNumber ~ (spacer ~> wholeNumber <~ endOfLine) ~ boardResults <~ (endOfLine | eoi) ^^ { case ns ~ ew ~ rs => Pickup(ns.toInt, ew.toInt, rs) }

	// XXX plays parser yields a list of Play objects where each play is terminated by a endOfLine.
	def plays: Parser[List[Play]] = rep(play <~ endOfLine)

	// XXX play parser yields a Play object and must be two integer numbers followed by a result
	def play: Parser[Play] = (opt(spacer) ~> wholeNumber <~ spacer) ~ (wholeNumber <~ spacer) ~ result ^^ { case n ~ e ~ r => Play(Try(n.toInt), Try(e.toInt), r) }

	// XXX boardResults parser yields a list of BoardResult objects where each result is terminated by a endOfLine.
	def boardResults: Parser[List[BoardResult]] = rep(boardResult <~ endOfLine)

	// XXX boardResult parser yields a BoardResult
	def boardResult: Parser[BoardResult] = opt(spacer) ~> (wholeNumber <~ spacer) ~ result ^^ { case n ~ r => BoardResult(n.toInt, r) }

	// XXX result parser yields a PlayResult object and must be either a number (a bridge score) or a string such as DNP or A[+-]
	def result: Parser[PlayResult] = (wholeNumber | "DNP" | regex("""A[\-\+]?""".r) | failure("result")) ^^ (s => PlayResult(s))

	// XXX title parser yields a String and must be a String not including a endOfLine
	def title: Parser[String] = """[^\r\n]+""".r

	def endOfLine: Parser[String] = s""" *$newline""".r | """ *\n""".r

	private def spacer: Parser[String] = """[ \t]*""".r

	private def sectionIdentifier: Parser[String] = """[A-Z]{1,2}""".r

	private val newline = sys.props("line.separator")

	private val eoi = """\z""".r // end of input
}

object RecapParser {
	def readEvent(s: Source): Try[Event] = if (s != null) {
		val p = new RecapParser
		val string = s.mkString
		try p.parseAll(p.event, string) match {
			case p.Success(e: Event, _) => scala.util.Success(e)
			case p.Failure(f, x) => scala.util.Failure(new Exception(s"parse failure: $f at $x"))
			case p.Error(f, x) => scala.util.Failure(new Exception(s"parse error: $f at $x"))
		}
		catch {
			case exception: Exception => System.err.println(s"yup got one: $exception"); Failure(exception)
		}
	} else Failure(new Exception("source is null"))
}