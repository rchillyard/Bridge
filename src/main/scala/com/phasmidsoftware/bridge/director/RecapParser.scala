/*
 * Copyright (c) 2019. Phasmid Software
 */
package com.phasmidsoftware.bridge.director

import scala.io.Source
import scala.language.postfixOps
import scala.util.*
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * RecapParser will parse a String as either an event, section, preamble, pair, traveler, play, or result, etc.
  *
  * @param delimiter this is an additional delimiter(s) above and beyond space and tab that are always supported.
  */
class RecapParser(delimiter: String = "") extends JavaTokenParsers {

  // XXX event parser yields an Event and is a title followed by a list of sections
  def event: Parser[Event] = (title <~ endOfLine) ~ rep(section) <~ opt(eoi) ^^
    { case p ~ ss => Event(p, ss) }

  // XXX section parser yields a Section and is a preamble followed by a list of travelers
  def section: Parser[Section] = preamble ~ travelers ~ pickups ^^
    { case p ~ ts ~ ps => Section(p, ts, ps) }

  // XXX (section) preamble parser yields a Preamble and is one or two letters, endOfLine, followed by a list of pair results, each on its own line.
  def preamble: Parser[Preamble] = (sectionIdentifier ~ opt(spacer ~> modifier) <~ endOfLine) ~ pairs ^^
    { case t ~ wo ~ ps => Preamble(t, wo, ps) }

  // XXX a modifier consisting of at least one capital letter
  private def modifier: Parser[String] = """[A-Z]+""".r | failure("modifier should be one or more upper-case characters")

  // XXX list of pairs, each terminated by a endOfLine
  def pairs: Parser[Seq[Pair]] = rep(pair <~ endOfLine)

  // XXX pair parser yields a Players object and is a number followed by "N" or "E" followed by two full names, each terminated by a period
  def pair: Parser[Pair] = (wholeNumber <~ spacer) ~ opt(direction <~ spacer) ~ playerPlayer ^^
    { case n ~ d ~ p => Pair(n.toInt, d, p._1 -> p._2) }

  // XXX pair parser yields a tuple of Player objects and is a string, possibly including space characters but not including & or endOfLine.
  def playerPlayer: Parser[Player ~ Player] = (player <~ ampersand) ~ player

  // XXX player parser yields a Player object and is at least one character that is neither & nor a newline char
  def player: Parser[Player] = """\s*\w[^\r\n&]*""".r ^^
    (s => Player(s.trim, -1)) // TODO do this properly

  // XXX travelers, each terminated by a endOfLine
  def travelers: Parser[Seq[Traveler]] = rep(traveler)

  // XXX traveler parser yields a Traveler object and must start with a "T" and end with a blank line. In between is a list of Play objects
  def traveler: Parser[Traveler] =
    opt(spacer) ~> "T" ~> spacer ~> (wholeNumber <~ endOfLine) ~ plays <~ terminator ^^
      { case b ~ ps => Traveler(Try(b.toInt), ps) }

  // XXX pickups, each terminated by a endOfLine
  def pickups: Parser[Seq[Pickup]] = rep(pickup)

  // XXX pickup parser yields a Pickup object and must start with a "P" and end with a blank line. In between is a list of Play objects
  def pickup: Parser[Pickup] =
    opt(spacer) ~> "P" ~> spacer ~> wholeNumber ~ (spacer ~> wholeNumber <~ endOfLine) ~ boardResults <~ terminator ^^
      { case ns ~ ew ~ rs => Pickup(ns.toInt, ew.toInt, rs) }

  // XXX plays parser yields a list of Play objects where each play is terminated by a endOfLine.
  def plays: Parser[Seq[Play]] = rep(play <~ endOfLine)

  // XXX play parser yields a Play object and must be two integer numbers followed by a result
  def play: Parser[Play] =
    (opt(spacer) ~> wholeNumber <~ spacer) ~ (wholeNumber <~ spacer) ~ result ^^
      { case n ~ e ~ r => Play(Try(n.toInt), Try(e.toInt), r) }

  // XXX boardResults parser yields a list of BoardResult objects where each result is terminated by a endOfLine.
  private def boardResults: Parser[Seq[BoardResult]] = rep(boardResult <~ terminator)

  // XXX boardResult parser yields a BoardResult
  private def boardResult: Parser[BoardResult] = opt(spacer) ~> (wholeNumber <~ spacer) ~ result ^^
    { case n ~ r => BoardResult(n.toInt, r) }

  /**
    * XXX result parser yields a PlayResult object and must be either a number (a bridge score) or a string such as (case-insensitive) DNP or A[+-]
    * NOTE that the the A annotations are NOT case-insensitive
    *
    * @return a Parser[PlayResult].
    */
  def result: Parser[PlayResult] = (wholeNumber | "(?i)DNP".r | regex("""A[\-+]?""".r) | failure("play result should be a number, A[+-], or DNP")) ^^
    (s => PlayResult(s))

  /**
    * Method to create a parser for an event title.
    *
    * @return Parser[String] which looks for a non-empty string which does not contain end-of-line characters.
    */
  def title: Parser[String] = """[^\r\n]+""".r | failure("not a proper event title")

  /**
    * Method to create a parser for end of line.
    *
    * @return a Parser[String]
    */
  def endOfLine: Parser[String] = rep(separator) ~> (s"""$newline""".r | """\n""".r | """\r\n""".r | failure("not a proper line-end"))

  override def skipWhitespace: Boolean = false

  // TODO sort out the white space and CSV delimiters properly.
  override val whiteSpace: Regex = """[\t ]+""".r

  private val separator: Regex = s"""[,\t|$delimiter]""".r // XXX these are the other likely CSV file delimiters which might terminate a line.

  private def terminator: Parser[String] = endOfLine | eoi | failure("not properly terminated")

  private def spacer: Parser[String] = whiteSpace | delimiter | failure("not a spacer--should be space, tab, or custom delimiter")

  private def sectionIdentifier: Parser[String] = """[A-Z]{1,2}""".r | failure("not a section identifier: should be one or two English letters")

  private def direction: Parser[String] = "E" | "N" | failure("direction must be E or N")

  private val newline = sys.props("line.separator")

  private val eoi = """\z""".r | failure("no end of input detected") // end of input

  private def ampersand: Parser[String] = """&""" | failure("& is missing")
}

object RecapParser {
  def readEvent(s: Source, delimiter: String = ""): Try[Event] = if (s != null) {
    val p = new RecapParser(delimiter)
    try p.parseAll(p.event, s.mkString) match {
      case p.Success(e: Event, _) => scala.util.Success(e)
      case p.Failure(f, x) => scala.util.Failure(new Exception(s"parse failure: $f at $x"))
      case p.Error(f, x) => scala.util.Failure(new Exception(s"parse error: $f at $x"))
    }
    catch {
      // TODO understand why this is a thrown exception instead of a parsing Failure.
      case exception: Exception => System.err.println(s"RecapParser.readEvent: exception thrown (and not caught) by parseAll on p.event: $exception"); Failure(exception)
    }
  }
  else Failure(new Exception("source is null"))
}
