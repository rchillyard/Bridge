package com.phasmidsoftware.bridge

import java.io.PrintWriter

import com.phasmid.laScala.fp.FP
import com.phasmid.laScala.values.Rational
import com.phasmidsoftware.output.Output

import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, _}

/**
  * Created by scalaprof on 4/12/16.
  */
object Score extends App {
  if (args.length > 0) {
    val filename = args.head
    doScore(Source.fromFile(filename)) match {
      case Success(o) => o.close()
      case Failure(x) => System.err.println(s"Score $filename threw an exception: $x")
    }
  }
  else System.err.println("Syntax: Score filename")

  def mpsAsString(r: Rational[Int], top: Int) = "%2.2f".format((r * top) toDouble)

  def mpsAsPercentage(r: Rational[Int], boards: Int): String =
    if (boards > 0) "%2.2f".format((r * 100 / boards).toDouble) + "%"
    else "infinity"

  def doScoreResource(resource: String, output: Output = Output(new PrintWriter(System.out))): Try[Output] =
    Option(getClass.getResourceAsStream(resource)) match {
      case Some(s) => doScore(Source.fromInputStream(s), output)
      case None => Failure(new Exception(s"doScoreResource: cannot open resource: $resource"))
    }

  def doScoreFromFile(filename: String, output: Output = Output(new PrintWriter(System.out))): Try[Output] = doScore(Source.fromFile(filename), output)

  def doScore(source: BufferedSource, output: Output = Output(new PrintWriter(System.out))): Try[Output] = {

    def getResultsForDirection(k: Preamble, r: Result, top: Int): Output = {
      def resultDetails(s: (Int, (Rational[Int], Int))): Output = Output(s"${s._1} : ${Score.mpsAsString(s._2._1, top)} : ${Score.mpsAsPercentage(s._2._1, s._2._2)} : ${k.getNames(r.isNS, s._1)}").insertBreak

      Output(r.card.toSeq.sortBy(_._2._1).reverse)(resultDetails)
    }

    def getResults(k: Preamble, r: Result): Output = Output(s"Results for direction: ${if (r.isNS) "N/S" else "E/W"}").insertBreak :+ getResultsForDirection(k, r, r.top)

    def eventResults(e: Event, k: Preamble, rs: Seq[Result]): Output = {
      val z = for (r <- rs) yield getResults(k, r)
      Output(s"Section ${k.identifier}").insertBreak ++ z :+
        "=====================================================\n" :+
        "=====================================================\n" :+
        e
    }

    val ey = RecapParser.readEvent(source)

    for (e <- ey) yield (output :+ e.title).insertBreak ++ (for ((k, rs) <- e.createResults) yield eventResults(e, k, rs))
  }
}

case class Event(title: String, sections: Seq[Section]) {
  if (sections.isEmpty)
    System.err.println("Warning: there are no sections in this event")

  override def toString: String = {
    val result = StringBuilder.newBuilder
    result.append(s"$title\n")
    for (s <- sections) result.append(s"$s\n")
    result.toString
  }

  def createResults: Map[Preamble, Seq[Result]] = (for (s <- sections) yield s.preamble -> s.createResults).toMap
}

case class Section(preamble: Preamble, travelers: Seq[Traveler]) {
  if (travelers.isEmpty)
    System.err.println(s"Warning: there are no travelers in this section: $preamble")

  override def toString: String = {
    val result = StringBuilder.newBuilder
    result.append(s"$preamble\n")
    for (t <- travelers) result.append(s"$t\n")
    result.toString
  }

  def createResults: Seq[Result] = {
    val top = calculateTop
    val recap: Seq[Matchpoints] = for (t <- travelers; m <- t.matchpointIt) yield m

    def all(n: Int, dir: Boolean): Seq[Rational[Int]] = recap.filter { m => m.matchesPair(n, dir) } flatMap { m => m.getMatchpoints(dir) }

    def total(d: Boolean): Seq[(Int, (Rational[Int], Int))] = for (p <- preamble.pairs; x = all(p.number, d)) yield p.number -> (x.sum, x.size)
    for (d <- Seq(true,false)) yield Result(d, top, total(d).toMap)
  }
  def calculateTop: Int = {
    val tops: Seq[Int] = for (t <- travelers) yield t.top
    val theTop = tops.distinct
    if (theTop.size != 1) println(s"Warning: not all boards have been played the same number of times: $tops")
    theTop.head
  }
}

/**
  * This represents the "preamble" to a section of an event.
  *
  * @param identifier the section identifier (a single or double upper-case letter)
  * @param pairs a list of the pairs in this section
  */
case class Preamble(identifier: String, maybeModifier: Option[String], pairs: Seq[Pair]) {
  if (pairs.isEmpty)
    System.err.println(s"Warning: there are no players in this section: $identifier")

  def getNames(ns: Boolean, n: Int): String = pairs.filter { p => p.number == n } map { p => p.brief } head

  override def toString: String = {
    val result = StringBuilder.newBuilder
    result.append(s"$identifier\n")
    for (p <- pairs) result.append(s"$p\n")
    result.toString
  }
}

case class Pair(number: Int, direction: String, players: (Player, Player)) {
  override def toString = s"$number$direction: $brief"

  def brief = s"${players._1} & ${players._2}"
}

/**
  * CONSIDER: splitting into first and last so that abbreviations can be used.
  *
  * @param name the name of the player
  */
case class Player(name: String) {
  override def toString: String = name
}

/**
  * This is the complete results for a particular direction
  *
  * @param isNS true if this result is for N/S; false if for E/W
  * @param top  top on a board
  * @param card a map of tuples containing total score and number of boards played, indexed by the pair number
  */
case class Result(isNS: Boolean, top: Int, card: Map[Int, (Rational[Int], Int)])

/**
  * This is the matchpoint result for one encounter (of NS/EW/Board).
  *
  * @param ns NS pair #
  * @param ew EW pair #
  * @param result the table result
  * @param mp the matchpoints earned by ns for this encounter
  * @param top the maximum number of matchpoints possible
  */
case class Matchpoints(ns: Int, ew: Int, result: PlayResult, mp: Option[Rational[Int]], top: Int) {
  def matchesPair(n: Int, dir: Boolean): Boolean = if (dir) n == ns else n == ew

  def getMatchpoints(dir: Boolean): Iterable[Rational[Int]] = if (dir) mp else invert

  override def toString: String = mp match {
    case Some(x) => s"NS: $ns, EW: $ew, score: $result, MP: ${Score.mpsAsString(x,top)}"
    case _ => ""
  }

  private def invert = mp map { r => -(r - 1) }

}

/**
  * This is the traveler for a specific board (in a specific, unnamed, section)
 *
  * @param board number
  * @param ps plays
  */
case class Traveler(board: Int, ps: Seq[Play]) {
  def isPlayed: Boolean = ps.nonEmpty
  // Calculate the ideal top -- including any Average or DNP scores:
  private[bridge] def top = ps.size - 1
  def matchpointIt: Seq[Matchpoints] = for (p <- ps) yield Matchpoints(p.ns,p.ew,p.result,p.matchpoints(this),top)

  override def toString: String = {
    val result = StringBuilder.newBuilder
    result.append (s"Board: $board with ${ps.size} plays\n")
    for (m <- matchpointIt) result.append(s"$m\n")
    result.toString
  }

  def matchpoint(x: Play): Option[Rational[Int]] = if (isPlayed) {
      val isIs = (for (p <- ps; if p != x; io = p.compare(x.result); i <- io) yield (i,2)) unzip;
      Some(Rational.normalize(isIs._1.sum,isIs._2.sum))
    }
    else None
}

object Traveler {
  def apply(it: Try[Int], ps: Seq[Play]): Traveler = {
    val tt = for (i <- it) yield Traveler(i,ps)
    tt.recover{case x => println(s"Exception: $x");Traveler(0,Seq())}.get
  }
}

/**
  * This is a particular play of an (unspecified) board from an (unspecified) section.
 *
  * @param ns NS pair number
  * @param ew EW pair number
  * @param result the table result
  */
case class Play(ns: Int, ew: Int, result: PlayResult) {
  override def toString = s"NS: $ns, EW: $ew, score: $result"
  def compare(x: PlayResult): Option[Int] = result match {
    case PlayResult(Right(y)) => x match {
      case PlayResult(Right(z)) => Some(Integer.compare(z,y)+1)
      case _ => None
    }
    case _ => None
  }

  def matchpoints(t: Traveler): Option[Rational[Int]] = {
    result match {
      case PlayResult(Right(_)) => t.matchpoint(this)
      case PlayResult(Left("A-")) => Some(Rational(2,5))
      case PlayResult(Left("A")) => Some(Rational(1,2))
      case PlayResult(Left("A+")) => Some(Rational(3,5))
      case _ => None // this accounts for the DNP case
    }
  }
}

object Play {
  def apply(ns: Try[Int], ew: Try[Int], result: PlayResult): Play = {
    val z = for (x <- ns; y <- ew) yield Play(x,y,result)
    z.recover{case x => println(s"Exception: $x");Play(0,0,PlayResult.error("no match"))}.get
  }
}

/**
  * This is a play result, that's to say either a bridge score (+ or - according to what NS scored)
  * OR a code.
  *
  * @param r Either: an integer (multiple of 10), Or: one of the following:
  *          DNP: did not play
  *          A+: N/S got Average plus (60%) and E/W Average minus
  *          A: both sides got Average (50%)
  *          A-: N/S got Average minus (40%) and E/W Average plus
  *
  */
case class PlayResult(r: Either[String,Int]) {
  override def toString: String = r match {
    case Left(x) => x
    case Right(x) => x.toString
  }
}

object PlayResult {
  def apply(s: String): PlayResult = {
    val z = FP.sequence[Int](Try(s.toInt)) match {
      case Left(_) => Left(s) // we ignore the exception because it is probably just a non-integer
      case Right(r) => Right(r)
    }
    PlayResult(z)
  }
  def error(s: String) = PlayResult(Left(s))
}

/**
  * RecapParser will parse a String as either an event, section, preamble, pair, traveler, play or result.
  */
class RecapParser extends JavaTokenParsers {
  override def skipWhitespace: Boolean = false

  // XXX event parser yields an Event and is a title followed by a list of sections
  def event: Parser[Event] = (title <~ endOfLine) ~ rep(section) <~ opt(eoi) ^^ { case p ~ ss => Event(p, ss) }

  // XXX section parser yields a Section and is a preamble followed by a list of travelers
  def section: Parser[Section] = preamble ~ travelers ^^ { case p ~ ts => Section(p, ts) }

  // XXX travelers, each terminated by a endOfLine
  def travelers: Parser[List[Traveler]] = rep(traveler)

  // XXX (section) preamble parser yields a Preamble and is one or two letters, endOfLine, followed by a list of pair results, each on its own line.
  def preamble: Parser[Preamble] = (sectionIdentifier ~ opt(spacer ~> modifier) <~ endOfLine) ~ pairs ^^ { case t ~ wo ~ ps => Preamble(t, wo, ps) }

  // XXX a modifier consisting of at least one capital letter
  def modifier: Parser[String] =
    """[A-Z]+""".r

  // XXX list of pairs, each terminated by a endOfLine
  def pairs: Parser[List[Pair]] = rep(pair <~ endOfLine)

  // XXX pair parser yields a Players object and is a number followed by "N" or "E" followed by two full names, each terminated by a period
  def pair: Parser[Pair] = (wholeNumber <~ spacer) ~ ("E" | "N") ~ playerPlayer ^^ { case n ~ d ~ p => Pair(n.toInt, d, p._1 -> p._2) }

  // XXX pair parser yields a tuple of Player objects and is a string, possibly including space characters but not including & or endOfLine.
  def playerPlayer: Parser[Player ~ Player] = (player <~"""&""") ~ player

  // XXX player parser yields a Player object and is at least one character that is neither & nor a newline char
  def player: Parser[Player] =
    """\s*\w[^\r\n&]*""".r ^^ (s => Player(s.trim))

  // XXX traveler parser yields a Traveler object and must start with a "T" and end with a blank line. In between is a list of Play objects
  def traveler: Parser[Traveler] = opt(spacer) ~> "T" ~> spacer ~> (wholeNumber <~ endOfLine) ~ plays <~ endOfLine ^^ { case b ~ r => Traveler(Try(b.toInt), r) }

  // XXX plays parser yields a list of Play objects where each play is terminated by a endOfLine.
  def plays: Parser[List[Play]] = rep(play <~ endOfLine)

  // XXX play parser yields a Play object and must be two integer numbers followed by a result
  def play: Parser[Play] = (opt(spacer) ~> wholeNumber <~ spacer) ~ (wholeNumber <~ spacer) ~ result ^^ { case n ~ e ~ r => Play(Try(n.toInt), Try(e.toInt), r) }

  // XXX result parser yields a PlayResult object and must be either a number (a bridge score) or a string such as DNP or A[+-]
  def result: Parser[PlayResult] = (wholeNumber | "DNP" | regex("""A[\-\+]?""".r) | failure("result")) ^^ (s => PlayResult(s))

  // XXX title parser yields a String and must be a String not including a endOfLine
  def title: Parser[String] =
    """[^\r\n]+""".r

  def endOfLine: Parser[String] = s""" *$newline""".r | """ *\n""".r

  private def spacer: Parser[String] = """ *""".r

  private def sectionIdentifier: Parser[String] = """[A-Z]{1,2}""".r

  private val newline = sys.props("line.separator")

  private val eoi = """\z""".r // end of input
}

object RecapParser {
  def readEvent(s: Source): Try[Event] = if (s != null) {
    val p = new RecapParser
    val string = s.mkString
    p.parseAll(p.event, string) match {
      case p.Success(e: Event, _) => scala.util.Success(e)
      case p.Failure(f, x) => scala.util.Failure(new Exception(s"parse failure: $f at $x"))
      case p.Error(f, x) => scala.util.Failure(new Exception(s"parse error: $f at $x"))
    }
  } else Failure(new Exception("source is null"))
}