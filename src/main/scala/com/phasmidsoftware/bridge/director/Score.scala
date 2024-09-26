/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.output.{Using, Util}
import com.phasmidsoftware.util.{Output, Outputable}

import java.io.PrintWriter
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.util.{Failure, Success, _}

/**
  * Created by scalaprof on 4/12/16.
  *
  */
object Score extends App {

  if (args.length > 0)
    doScoreFromName(isResource = false, args.head) match {
      case Success(o) => o.close()
      case Failure(x) => System.err.println(s"Score ${args.mkString} threw an exception: $x")
    }
  else System.err.println("Syntax: Score filename")

  def doScoreFromName(isResource: Boolean, name: String, output: Output = Output(new PrintWriter(System.out))): Try[Output] = Using(
    if (isResource) Source.fromResource(name) else Source.fromFile(name)
  ) { s => doScore(s, output) }

  // TODO use the methods in Result
  def doScoreResource(resource: String, output: Output = Output(new PrintWriter(System.out))): Try[Output] =
    Option(getClass.getResourceAsStream(resource)) match {
      case Some(s) => doScore(Source.fromInputStream(s), output)
      case None => Failure(ScoreException(s"doScoreResource: cannot open resource: $resource"))
    }

  def doScoreFromFile(filename: String, output: Output = Output(new PrintWriter(System.out))): Try[Output] = doScore(Source.fromFile(filename), output)

  // TESTME
  def doScore(source: BufferedSource, output: Output = Output(new PrintWriter(System.out))): Try[Output] = {

    implicit val separator: Output = Output.empty.insertBreak()

    def eventResults(e: Event, p: Preamble, rs: Seq[Result]): Output = {
      val z = for (r <- rs) yield r.getResults(p.getNames)
      (Output(s"Section ${p.identifier}") ++ z :+
        "=====================================================\n" :+
        "=====================================================\n") ++
        e.output(Output.empty)
    }

    val ey = RecapParser.readEvent(source)

    for (e <- ey) yield (output :+ e.title).insertBreak ++ (for ((p, rs) <- e.createResults) yield eventResults(e, p, rs))
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

  /**
    * Method to render a Rational (r).
    *
    * CONSIDER This method belongs in Rational.
    *
    * @param r a Rational.
    * @return r rendered in 5 spaces.
    */
  //noinspection SpellCheckingInspection
  def rationalToString(r: Rational): String = r match {
    case Rational(x, Rational.bigOne) => f"$x%2d.00"
    case Rational(_, Rational.bigZero) => "infty"
    case _ => f"${r.toDouble}%5.2f"
  }
}

/**
  * Class to represent an Event.
  *
  * @param title    the event's title.
  * @param sections a sequence of sections.
  */
case class Event(title: String, sections: Seq[Section]) extends Outputable[Unit] {
  if (sections.isEmpty)
    System.err.println("Warning: there are no sections in this event")

  def createResults: Map[Preamble, Seq[Result]] = (for (s <- sections) yield s.preamble -> s.createResults).toMap

  /**
    * Method to output this object (and, recursively, all of its children).
    *
    * @param output the output to append to.
    * @param xo     an optional value of X, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output = (output :+ title).insertBreak ++ Output.apply(sections)(s => s.output(Output.empty))
}

/**
  * Class to represent a section of an event.
  *
  * @param preamble  the preamble describing this section.
  * @param travelers a sequence of travelers (maybe be empty of all pickup slips are used).
  */
case class Section(preamble: Preamble, travelers: Seq[Traveler]) extends Outputable[Unit] {

  lazy val createResults: Seq[Result] = {
    val top = calculateTop
    preamble.maybeModifier match {
      case Some(Preamble.SingleWinner) => Seq(Result(None, top, getSwResults))
      case _ => for (d <- Seq(true, false)) yield Result(Some(d), top, total(d).toMap)
    }
  }

  lazy val calculateTop: Int = {
    val tops: Seq[Int] = for (t <- travelers) yield t.top
    val theTop = tops.distinct
    if (theTop.size != 1) System.err.println(s"Warning: not all boards have been played the same number of times: $tops")
    theTop.head
  }

  /**
    * Method to output this object (and, recursively, all of its children).
    *
    * @param output the output to append to.
    * @param xo     an optional value of X, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output = travelers.sorted.foldLeft(output :+ s"$preamble\n")((o, t) => o ++ t.output(Output.empty))

  private lazy val recap: Seq[Matchpoints] = for (t <- travelers; m <- t.matchpointIt) yield m

  private def all(n: Int, dir: Boolean): Seq[Option[Rational]] = recap.filter(_.matchesPair(n, dir)) map (_.getMatchpoints(dir))

  private def total(d: Boolean): Seq[Pos] = for {p <- preamble.pairs
                                                 ros = all(p.number, d)
                                                 } yield p.number -> Card(ros)

  private def sum(iCs: Seq[Pos]): Card = {
    val (is, cs) = iCs.unzip
    if (is.distinct.length > 1) throw ScoreException(s"logic error: sum: indices should be the same")
    cs.foldLeft(Card(0, 0, 0))(_ + _)
  }

  private lazy val getSwResults: Map[Int, Card] =
    for ((i, i_cs) <- total(true) ++ total(false) groupBy { case (i, _) => i }) yield i -> sum(i_cs)
}

object Section {
  def apply(preamble: Preamble, travelers: Seq[Traveler], pickups: Seq[Pickup]): Section = {
    val travelerMap: Map[Int, Traveler] = (travelers map (t => t.board -> t)).toMap
    val boardPlays: Seq[BoardPlay] = for (p <- pickups; b <- p.asBoardPlays) yield b
    val tIm = boardPlays.foldLeft(travelerMap)((m, b) => b.addTo(m))
    apply(preamble, tIm.values.toList)
  }
}

/**
  * This represents the "preamble" to a section of an event.
  *
  * @param identifier    the section identifier (a single or double upper-case letter)
  * @param maybeModifier is an optional String denoting the type of movement (SW for single-winner, etc.)
  * @param pairs         a list of the pairs in this section
  */
case class Preamble(identifier: String, maybeModifier: Option[String], pairs: Seq[Pair]) {
  if (pairs.isEmpty)
    System.err.println(s"Warning: there are no players in this section: $identifier")

  if (!pairs.forall(_.valid(maybeModifier))) throw ScoreException("pairs must have direction unless movement is single-winner")

  def getNames(ns: Option[Boolean], n: Int): String = {
    val ws: Seq[String] = pairs.filter { p => p.number == n } map { p => p.brief }

    maybeModifier match {
      case Some(Preamble.SingleWinner) =>
        ws.head
      case _ =>
        val wt = Util.asTuple2(ws)
        ns match {
          case Some(b) => if (b) wt._1 else wt._2
          case _ => throw ScoreException("logic error in Preamble.getNames")
        }

    }
  }

  override def toString: String = {
    val result = new StringBuilder
    result.append(s"$identifier\n")
    for (p <- pairs) result.append(s"$p\n")
    result.toString
  }
}

object Preamble {
  val SingleWinner: String = "SW"
}

/**
  * Class to represent a pair.
  *
  * @param number         the pair number.
  * @param maybeDirection the (optional) direction (assuming a Mitchell movement).
  * @param players        the players who make up this pair (N, E first).
  */
case class Pair(number: Int, maybeDirection: Option[String], players: (Player, Player)) {

  override def toString: String = s"""$number$direction\t$brief"""

  // CONSIDER making this more elegant
  def valid(x: Option[String]): Boolean = x match {
    case Some(Preamble.SingleWinner) => "N" == (maybeDirection getOrElse "N")
    case _ => maybeDirection.isDefined
  }

  lazy val brief: String = s"${players._1} & ${players._2}"

  private lazy val direction = maybeDirection getOrElse ""
}

/**
  * Class to represent a player.
  *
  * CONSIDER: splitting into first and last so that abbreviations can be used.
  *
  * @param name the name of the player
  */
case class Player(name: String) {
  override def toString: String = name
}

case class Card(totalMps: Rational, played: Int, notPlayed: Int) extends Ordered[Card] {

  def toStringMps(top: Int): String = Card.mpsAsString(scaledMps, top)

  def compare(that: Card): Int = percentage.compare(that.percentage)

  def +(c: Card): Card = Card(totalMps + c.totalMps, played + c.played, notPlayed + c.notPlayed)

  lazy val toStringPercent: String = Score.rationalToString(percentage) + "%"

  private lazy val percentage: Rational = Score.asPercent(totalMps, played)

  private def scaledMps = totalMps * (played + notPlayed) / played
}

object Card {
  def apply(ros: Seq[Option[Rational]]): Card = {
    val irs: Seq[Rational] = ros.flatten
    Card(irs.sum, irs.size, ros.size - irs.size)
  }

  def mpsAsString(r: Rational, top: Int): String = Score.rationalToString(r * top)
}

/**
  * This is the complete results for a particular direction
  *
  * @param isNS  (optional) Some(true) if this result is for N/S; Some(false) if for E/W; None for single winner.
  * @param top   top on a board
  * @param cards a map of tuples containing total score and number of boards played, indexed by the pair number
  */
case class Result(isNS: Option[Boolean], top: Int, cards: Map[Int, Card]) {

  /**
    * Method to get results, given a nameFunction
    *
    * @param nameFunction a function to yield a name, given an optional direction and a pair number.
    * @return Output
    */
  def getResults(nameFunction: (Option[Boolean], Int) => String): Output = Output(s"Results" + directionHeader).insertBreak ++ getResultsForDirection(nameFunction(isNS, _))

  private lazy val directionHeader = isNS match {
    case Some(d) => " for direction " + (if (d) "N/S" else "E/W")
    case None => ""
  }

  private def getResultsForDirection(nameFunction: Int => String) = {
    def resultDetails(s: (Pos, Int)): Output = {
      val ((pairNumber, card), rank) = s
      Output(s"""$rank\t$pairNumber\t${card.toStringMps(top)}\t${card.toStringPercent}\t${nameFunction(pairNumber)}""").insertBreak()
    }

    val header = Output(s"""Pos\tPair\tMPs\tPercent\tNames\n""")
    val cardsInOrder: Seq[Pos] = cards.toList.sortBy(_._2).reverse
    val keyFunction: Pos => Rational = t => t._2.totalMps
    val psRM: Map[Rational, Seq[Pos]] = cardsInOrder.groupBy[Rational](keyFunction)
    val psRXs: Seq[((Rational, Seq[Pos]), Int)] = psRM.toSeq.sortBy(_._1).reverse.zipWithIndex
    val xPs: Seq[(Pos, Int)] = for {((_, ps), x) <- psRXs; p <- ps} yield (p, x + 1)
    Output.foldLeft(xPs)(header)(_ ++ resultDetails(_))
  }
}

/**
  * This is the matchpoint result for one boardResult (of NS/EW/Board).
  *
  * @param ns     NS pair #
  * @param ew     EW pair #
  * @param result the table result
  * @param mp     (optionally) the matchpoints earned by ns for this boardResult
  * @param top    the maximum number of matchpoints possible
  */
case class Matchpoints(ns: Int, ew: Int, result: PlayResult, mp: Option[Rational], top: Int) {
  def matchesPair(n: Int, dir: Boolean): Boolean = if (dir) n == ns else n == ew

  def getMatchpoints(dir: Boolean): Option[Rational] = if (dir) mp else invert

  // CONSIDER extending Outputable and putting this logic into output method.
  override def toString: String = mp match {
    case Some(x) => s"""$ns\t$ew\t$result\t${Card.mpsAsString(x, top)}"""
    case _ => ""
  }

  private def invert = mp map { r => -(r - 1) }

}

/**
  * This is a traveler for a specific board (in a specific, unnamed, section).
  * We usually report boards either by travelers or pickup slips.
  * It is however possible to mix these up.
  *
  * @param board the board number.
  * @param ps    the plays.
  */
case class Traveler(board: Int, ps: Seq[Play]) extends Outputable[Unit] with Ordered[Traveler] {
  def isPlayed: Boolean = ps.nonEmpty

  // Calculate the ideal top -- including any Average or DNP scores:
  private[bridge] def top = ps.size - 1

  def matchpointIt: Seq[Matchpoints] = for (p <- ps) yield Matchpoints(p.ns, p.ew, p.result, p.matchpoints(this), top)

  def matchpoint(x: Play): Option[Rational] = if (isPlayed) {
    val isIs = (for (p <- ps; if p != x; io = p.compare(x.result); i <- io) yield (i, 2)) unzip;
    Some(Rational(isIs._1.sum, isIs._2.sum))
  }
  else None

  /**
    * Method to add a Play into this Traveler.
    *
    * @param play the play to be added
    * @return the new combined Traveler.
    */
  def :+(play: Play): Traveler = Traveler(board, ps :+ play)

  override def compare(that: Traveler): Int = board - that.board

  /**
    * Method to output this object (and, recursively, all of its children).
    *
    * @param output the output to append to.
    * @param xo     an optional value of X, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output = {
    val result = new StringBuilder
    result.append(s"Board: $board with ${ps.size} plays\n")
    result.append(s"""NS pair\tEW pair\tNS score\tNS MPs\n""")
    for (m <- matchpointIt) result.append(s"$m\n")
    output :+ result.toString
  }
}

object Traveler {
  def apply(it: Try[Int], ps: Seq[Play]): Traveler = {
    val tt = for (i <- it) yield Traveler(i, ps)
    tt.recover { case x => System.err.println(s"Exception: $x"); Traveler(0, List()) }.get
  }
}

/**
  * Class to describe a board result.
  *
  * CONSIDER renaming this case class (and parser methods)
  *
  * @param board  the board number.
  * @param result the result.
  */
case class BoardResult(board: Int, result: PlayResult) {
  override def toString: String = s"$board: $result"
}

/**
  * Class to describe a pickup slip.
  * NOTE: board numbers are not required to be a consecutive set.
  *
  * @param ns     the N/S pair.
  * @param ew     the E/W pair.
  * @param boards a sequence of BoardResults which are written on this pickup slip.
  */
case class Pickup(ns: Int, ew: Int, boards: Seq[BoardResult]) {
  def asBoardPlays: Seq[BoardPlay] = for (e <- boards) yield BoardPlay(e.board, Play(ns, ew, e.result))

  override def toString: String = s"Pickup: $ns vs $ew: ${boards.mkString(", ")}"
}

/**
  * Class to describe the play on a board.
  *
  * CONSIDER merging this class with BoardResult.
  *
  * @param board the board number.
  * @param play  the play.
  */
case class BoardPlay(board: Int, play: Play) {
  def addTo(travelerMap: Map[Int, Traveler]): Map[Int, Traveler] = {
    val entry = travelerMap.get(board)
    val traveler = entry.getOrElse(Traveler(board, Nil))
    travelerMap + (board -> (traveler :+ play))
  }

  override def toString: String = s"$board: $play"
}

/**
  * This is a particular play of an (unspecified) board from an (unspecified) section.
  *
  * @param ns     NS pair number
  * @param ew     EW pair number
  * @param result the table result
  */
case class Play(ns: Int, ew: Int, result: PlayResult) {
  override def toString: String = s"$ns vs $ew: $result"

  def compare(x: PlayResult): Option[Int] = result match {
    case PlayResult(Right(y)) => x match {
      case PlayResult(Right(z)) => Some(Integer.compare(z, y) + 1)
      case _ => None
    }
    case _ => None
  }

  def matchpoints(t: Traveler): Option[Rational] = result.matchpoints(t.matchpoint(this))

}

object Play {
  def apply(ns: Try[Int], ew: Try[Int], result: PlayResult): Play = {
    val z = for (x <- ns; y <- ew) yield Play(x, y, result)
    z.recover { case x => System.err.println(s"Exception: $x"); Play(0, 0, PlayResult.error("no match")) }.get
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
case class PlayResult(r: Either[String, Int]) {
  /**
    * Method to get the matchpoints for this PlayResult
    *
    * @param f call-by-name value of the matchpoints where the result is an an Int
    * @return an optional Rational
    */
  def matchpoints(f: => Option[Rational]): Option[Rational] = r match {
    case Right(_) => f
    case Left("A-") => Some(Rational(2, 5))
    case Left("A") => Some(Rational(1, 2))
    case Left("A+") => Some(Rational(3, 5))
    case _ => None // this accounts for the DNP case
  }

  override def toString: String = r match {
    case Left(x) => x
    case Right(x) => x.toString
  }
}

object PlayResult {
  def apply(s: String): PlayResult = {
    val z = Try(s.toInt).toEither match {
      case Left(_) => Left(s) // we ignore the exception because it is probably just a non-integer
      case Right(r) => Right(r)
    }
    PlayResult(z)
  }

  def error(s: String): PlayResult = PlayResult(Left(s))
}

case class ScoreException(str: String) extends Exception(str)
