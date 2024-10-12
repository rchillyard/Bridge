/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.Card.mpsAsString
import com.phasmidsoftware.bridge.director.Matchpoints.rationalToString
import com.phasmidsoftware.bridge.director.Score.asPercent
import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.number.core.Rational.half
import com.phasmidsoftware.output.{Using, Util}
import com.phasmidsoftware.util.{Output, Outputable}

import java.io.{FileWriter, PrintWriter}
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.util.{Failure, Success, _}

/**
  * Created by scalaprof on 4/12/16.
  *
  */
object Score extends App {

  private lazy val fileWriter = new FileWriter("output.csv")
  private lazy val printWriter = new PrintWriter(System.out)
  private lazy val tabbedOutput: Output = Output(fileWriter)
  private lazy val untabbedOutput: Output = Output.untabbedWriter(printWriter, 6)
  // TODO set this to use untabbedOutput if you want all tabs turned into spaces.
  lazy val defaultOutput: Output = tabbedOutput

  doMain(tabbedOutput)

  // TESTME
  def doMain(output: Output): Unit = {
    if (args.length > 0)
      doScoreFromName(isResource = false, args.head, output) match {
        case Success(o) => o.close()
        case Failure(x) => System.err.println(s"Score ${args.mkString} threw an exception: $x")
      }
    else System.err.println("Syntax: Score filename")
  }

  def doScoreFromName(isResource: Boolean, name: String, output: Output = defaultOutput): Try[Output] = Using(
    if (isResource) Source.fromResource(name) else Source.fromFile(name)
  ) { s => doScore(s, output) }

  // TODO use the methods in Result
  def doScoreResource(resource: String, output: Output = defaultOutput): Try[Output] =
    Option(getClass.getResourceAsStream(resource)) match {
      case Some(s) => doScore(Source.fromInputStream(s), output)
      case None => Failure(ScoreException(s"doScoreResource: cannot open resource: $resource"))
    }

  def doScoreFromFile(filename: String, output: Output = defaultOutput): Try[Output] = doScore(Source.fromFile(filename), output)

  // TESTME
  def doScore(source: BufferedSource, output: Output = defaultOutput): Try[Output] = {

    implicit val separator: Output = Output.empty.insertBreak()

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

    val ey = RecapParser.readEvent(source)

    for (e <- ey; x = e.score) yield (output :+ x.title).insertBreak ++ (for ((p, rs) <- x.createResults) yield eventResults(x, p, rs, x.boards))
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

/**
  * Class to represent an Event.
  *
  * @param title    the event's title.
  * @param sections a sequence of sections.
  */
case class Event(title: String, sections: Seq[Section]) extends Outputable[Unit] {
  private val xs: Seq[Int] = sections map (_.boards) distinct

  if (sections.isEmpty)
    System.err.println("Warning: there are no sections in this event")

  if (xs.size != 1)
    System.err.println("Warning: sections played different numbers of boards")

  lazy val boards: Int = xs.headOption.getOrElse(0)

  def score: Event = copy(title, sections map (_.recap))

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
case class Section(preamble: Preamble, travelers: Seq[Traveler], maybeTop: Option[Int] = None) extends Outputable[Unit] {
  private val top = calculateTop

  lazy val boards: Int = travelers.size

  lazy val createResults: Seq[Result] = preamble.maybeModifier match {
    case Some(Preamble.SingleWinner) => Seq(Result(None, top, getSwResults))
    case _ => for {
      d <- Seq(true, false)
      totalMPs: Seq[Pos] = total(d)
      map: Map[Int, Card] = totalMPs.toMap
    } yield Result(Some(d), top, map)
  }

  /**
    * Method to calculate the top and to check that all boards have been entered.
    *
    * TODO the warning should only appear when a board-play is missing from the pickups (or travelers).
    * Currently, it's complaining even if there is a DNP entry.
    */
  lazy val calculateTop: Int = {
    val tops: Seq[(Int, Int)] = for (t <- travelers.sortBy(_.board)) yield (t.board, t.top)
    val theTop = tops.distinctBy(x => x._2)
    if (theTop.size != 1 && maybeTop.isEmpty)
      System.err.println(s"Warning: not all boards have been played the same number of times. The calculated tops are: $tops")
    theTop.head._2
  }

  /**
    * Method to output this object (and, recursively, all of its children).
    *
    * @param output the output to append to.
    * @param xo     an optional value of X, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output = travelers.sorted.foldLeft(output :+ s"$preamble\n")((o, t) => o ++ t.output(Output.empty))

  lazy val recap: Section = copy(travelers = travelers map { t => t.matchpointIt(top) }).copy(maybeTop = Some(top))

  private def all(n: Int, dir: Boolean): Seq[Option[Rational]] =
    for {
      t <- recap.travelers // CONSIDER invoking recap somewhere else
      ms <- t.maybeMatchpoints.toSeq // CONSIDER replace with matchpoints
      m <- ms.filter(_.matchesPair(n, dir))
    } yield m.getMatchpoints(dir)

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

/**
  * Class to represent a "Card" for a pair.
  *
  * CONSIDER why do we need the number of boards played and not played now that we are doing factoring of DNPs?
  *
  * @param totalMps  the total matchpoints earned but divided by the top.
  * @param played    the number of boards played.
  * @param notPlayed the number of boards not played.
  */
case class Card(totalMps: Rational, played: Int, notPlayed: Int) extends Ordered[Card] {

  def toStringMps(top: Int): String = mpsAsString(scaledMps, top)

  def compare(that: Card): Int = percentage.compare(that.percentage)

  def +(c: Card): Card = Card(totalMps + c.totalMps, played + c.played, notPlayed + c.notPlayed)

  lazy val toStringPercent: String = rationalToString(percentage) + "%"

  private lazy val percentage: Rational = asPercent(totalMps, played)

  private def scaledMps = totalMps * (played + notPlayed) / played
}

/**
  * Companion object to Card class.
  */
object Card {
  def apply(ros: Seq[Option[Rational]]): Card = {
    val irs: Seq[Rational] = ros.flatten
    Card(irs.sum, irs.size, ros.size - irs.size)
  }

  def mpsAsString(r: Rational, top: Int): String = rationalToString(r * top)
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

  /**
    * In the following, n = # players in each direction.
    * cards.size = n.
    * top = n - 1
    * The total matchpoints overall should be boards * sum{top}, i.e. n * (n - 1)) * boards / 2
    * The total matchpoints for a board should be B = sum (#top), i.e. 1/2 * n * (n - 1)
    * The total matchpoints for a board expressed as a fraction is F = n/2.
    *
    * An incomplete traveler with (n - 1) plays should result in total matchpoints of 1/2 * (n - 1) * (n - 2) = B * (n - 2) / n
    * Factoring by n / (n - 1) will result in B' = B * (n - 2) / (n - 1)
    * So, B - B' = 1/2 * {n * (n - 1) - (n - 2) / (n - 1)} = ???
    *
    * @param boards the number of boards
    * @return true if the matchpoint sums check out.
    */
  def checksum(boards: Int): Boolean = {
    val matchpoints: Rational = cards.map(_._2.totalMps).sum
    val n = cards.size
    val expected = Rational(n * boards, 2)
    val result = matchpoints == expected
    val direction = isNS match {
      case Some(b) if b => "NS"
      case Some(_) => "EW"
      case None => "SW"
    }
    if (!result)
      System.err.println(s"Total fractional matchpoints for this $direction result ($matchpoints) differ from what is expected for $boards boards and $n players ($boards * $n / 2 = $expected)")
    result
  }

  private def getResultsForDirection(nameFunction: Int => String) = {

    case class Psi(len: Int, rank: Int, ps: Seq[Pos])
    object Psi {
      def empty: Psi = Psi(0, 0, Nil)
    }

    def resultDetails(s: (Pos, Int, String)): Output = {
      val ((pairNumber, card), rank, suffix) = s
      Output(s"""$rank$suffix\t$pairNumber\t${card.toStringMps(top)}\t${card.toStringPercent}\t${nameFunction(pairNumber)}""").insertBreak()
    }

    val keyFunction: Pos => Rational = t => t._2.totalMps
    val ps: Seq[Pos] = cards.toList.sortBy(_._2).reverse
    val psRm: Map[Rational, Seq[Pos]] = ps.groupBy[Rational](keyFunction)
    val psRs: Seq[(Rational, Seq[Pos])] = psRm.toSeq.sortBy(_._1).reverse
    val psis: Seq[Psi] = psRs.scanLeft(Psi.empty) {
      case (Psi(l, _, _), (_, ps)) => Psi(l + ps.length, l, ps)
    }
    val xPs: Seq[(Pos, Int, String)] = for {Psi(_, r, ps) <- psis; x = if (ps.size > 1) "=" else " "; p <- ps} yield (p, r + 1, x)
    val header = Output(s"Rank\tPair\tMPs\tPercent\tNames\n")
    Output.foldLeft(xPs)(header)(_ ++ resultDetails(_))
  }
}

/**
  * This is the matchpoint result for one boardResult (of NS/EW/Board).
  *
  * @param ns     NS pair #
  * @param ew     EW pair #
  * @param result the table result
  * @param ro     (optionally) the matchpoints earned by ns for this boardResult
  * @param top    the tentative top on a board.
  * */
case class Matchpoints(ns: Int, ew: Int, result: PlayResult, ro: Option[Rational], top: Int) {
  def factorIfRequired(idealTop: Int): Matchpoints = if (top == idealTop) this else copy(ro = factorACBL(idealTop))

  def matchesPair(n: Int, dir: Boolean): Boolean = if (dir) n == ns else n == ew

  def getMatchpoints(dir: Boolean): Option[Rational] = if (dir) ro else invert

  override def toString: String = ro match {
    case Some(x) => s"""$ns\t$ew\t$result\t${Card.mpsAsString(x, top)}"""
    case _ => ""
  }

  private def factorACBL(idealTop: Int) = ro match {
    case None => Some(half)
    case Some(r) => Some(((r * top + half) * (idealTop + 1) / (top + 1) - half) / idealTop)
  }

  private def invert = ro map { r => -(r - 1) }
}

object Matchpoints {

  /**
    * Method to render a Rational (r).
    *
    * @param r a Rational.
    * @return r rendered in 5 spaces.
    */
  //noinspection SpellCheckingInspection
  def rationalToString(r: Rational): String = r match {
    case Rational(x, Rational.bigOne) => f"$x%2d.00"
    case Rational(_, Rational.bigZero) => "infty"
    case _ => r.renderApproximate(5, Some(2))
  }

}

/**
  * This is a traveler for a specific board (in a specific, unnamed, section).
  * We usually report boards either by travelers or pickup slips.
  * It is however possible to mix these up.
  *
  * @param board the board number.
  * @param ps    the plays.
  */
case class Traveler(board: Int, ps: Seq[Play], maybeMatchpoints: Option[Seq[Matchpoints]]) extends Outputable[Unit] with Ordered[Traveler] {
  lazy val isPlayed: Boolean = ps.nonEmpty

  // Count the plays, including average scores but not including DNP results
  private val n: Int = ps.count(p => p.result.matchpoints(Some(1)).isDefined)

  // Calculate the raw top
  private[bridge] lazy val top = n - 1

  def matchpointIt(idealTop: Int): Traveler = copy(maybeMatchpoints = Some(calculateMatchpoints(idealTop)))

  def matchpoints: Seq[Matchpoints] = maybeMatchpoints getOrElse calculateMatchpoints(top)

  def matchpoint(x: Play): Option[Rational] = if (isPlayed) {
    val isIs = (for (p <- ps; if p != x; i <- p.compare(x.result)) yield (i, 2)) unzip;
    Some(Rational(isIs._1.sum, isIs._2.sum))
  }
  else None

  /**
    * Method to add a Play into this Traveler.
    *
    * @param play the play to be added
    * @return the new combined Traveler.
    */
  def :+(play: Play): Traveler = Traveler(board, ps :+ play, None)

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
    for (m <- matchpoints) result.append(s"$m\n")
    output :+ result.toString
  }

  private def calculateMatchpoints(idealTop: Int): Seq[Matchpoints] =
    for (p <- ps) yield Matchpoints(p.ns, p.ew, p.result, p.matchpoints(this), top).factorIfRequired(idealTop)
}

/**
  * Companion object to Traveler class.
  */
object Traveler {
  def apply(it: Try[Int], ps: Seq[Play]): Traveler = {
    val tt = for (i <- it) yield Traveler(i, ps, None)
    tt.recover { case x => System.err.println(s"Exception: $x"); Traveler(0, List(), None) }.get
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
    val traveler = entry.getOrElse(Traveler(board, Nil, None))
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

  /**
    * Calculate the matchpoints for this Play in the context of the given Traveler (t)
    *
    * @param t the traveler (i.e., the board) on which we find this Play.
    * @return an optional Rational.
    */
  def matchpoints(t: Traveler): Option[Rational] = result.matchpoints(t.matchpoint(this))

}

/**
  * Companion object to Play class.
  */
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
    case Left("DNP") => None
    case _ => throw ScoreException(s"matchpoints: unrecognized result: $r")
  }

  override def toString: String = r match {
    case Left(x) => x
    case Right(x) => x.toString
  }
}

/**
  * Companion object to PlayResult class.
  */
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
