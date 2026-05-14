/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.Matchpoints.{mpsAsString, rationalToString}
import com.phasmidsoftware.bridge.director.Score.asPercent
import com.phasmidsoftware.gambit.util.{Output, Outputable}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.{half, zero}
import com.phasmidsoftware.output.Util

import scala.annotation.unused
import scala.language.postfixOps
import scala.util.*

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
  lazy val boards: Int = travelers.size

  private lazy val tables = preamble.pairs.size / 2 // NOTE: we assume that all tables that can be filled are filled.

  private lazy val top = calculateTop

  locally {
    lazy val _ = countResults
  }

  lazy val createResults: Seq[Result] = preamble.maybeModifier match {
    case Some(Preamble.SingleWinner) => Seq(Result(tables, None, top, getSwResults))
    case _ => for {
      d <- Seq(true, false)
      totalMPs: Seq[Pos] = total(d)
      map: Map[Int, Card] = totalMPs.toMap
    } yield Result(tables, Some(d), top, map)
  }

  /**
    * Method to calculate the top.
    *
    */
  lazy val calculateTop: Int = {
    val tops: Seq[(Int, Int)] = for (t <- travelers.sortBy(_.board)) yield (t.board, t.top)
    tops.distinctBy(x => x._2).head._2
  }

  /**
    * Method to calculate the top and to check that all boards have been entered.
    *
    * TODO the warning should only appear when a board-play is missing from the pickups (or travelers).
    */
  private lazy val countResults: Int = {
    val tops: Seq[(Int, Int)] = for (t <- travelers.sortBy(_.board)) yield (t.board, t.ps.count(p => p.result.exists))
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
  def output(output: Output, xo: Option[Unit] = None): Output = {
    if (boards > 0)
      System.out.println(s"Section ${preamble.identifier} has processed $boards boards")
    else
      System.err.println(s"Section ${preamble.identifier} has no results (no travelers)")
    travelers.sorted.foldLeft(output :+ s"$preamble\n")((o, t) => o ++ t.output(Output.empty))
  }

  lazy val recap: Section = {
    val result = copy(travelers = travelers map { t => t.matchpointIt(top) }).copy(maybeTop = Some(top))
    if (!result.boardsOK) println("At least one result needs checking")
    result
  }

  private def all(n: Int, dir: Boolean): Seq[(BoardResult, Option[Rational])] = // was Seq[Option[Rational]]
    for {
      t: Traveler <- recap.travelers // CONSIDER invoking recap somewhere else
      ms: Seq[Matchpoints] <- t.maybeMatchpoints.toSeq // CONSIDER replace with matchpoints
      m: Matchpoints <- ms.filter(_.matchesPair(n, dir))
      ro: Option[PlayResult] <- t.ps map (p => p.playResult(n, dir))
      r: PlayResult <- ro
      br: BoardResult = BoardResult(t.board, r)
    } yield (br, m.getMatchpoints(dir))

  private def total(d: Boolean): Seq[Pos] = for (p <- preamble.pairs) yield p.number -> Card(all(p.number, d))

  private def sum(iCs: Seq[Pos]): Card = {
    val (is, cs) = iCs.unzip
    if (is.distinct.length > 1) throw ScoreException(s"logic error: sum: indices should be the same")
    cs.foldLeft(Card(0, 0, 0, Nil))(_ + _)
  }

  private lazy val getSwResults: Map[Int, Card] =
    for ((i, i_cs) <- total(true) ++ total(false) groupBy { case (i, _) => i }) yield i -> sum(i_cs)

  private def boardsOK: Boolean = ((for (t <- travelers; p <- t.ps) yield t.board -> p) map { case (b, p) => p.checkScore(b) }).forall(b => b)
}

/**
  * Companion object to `Section`.
  */
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
    val ws: Seq[(Option[String], String)] = pairs.filter { p => p.number == n } map { p => p.maybeDirection -> p.brief }

    maybeModifier match {
      case Some(Preamble.SingleWinner) =>
        ws.head._2
      case _ =>
        val (maybeDirection, names) = ws.unzip
        val maybeN = maybeDirection map (_.getOrElse("") == "N") // CONSIDER making this case-insensitive
        ns match {
          case Some(b) =>
            val wt = Util.asTuple2(names, "Phantom pair", maybeN)("pair names")
            if (b) wt._1 else wt._2
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


/**
  * Companion object to `Preamble`.
  */
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
  * The reason is that we do factor the total matchpoints (and percentage) according to those values.
  * NOTE: when there are unplayed boards, they must be entered as DNP otherwise this mechanism doesn't work
  * (the percentages will still be correct but not the total matchpoints).
  *
  * @param totalMps  the total matchpoints earned but divided by the top.
  * @param played    the number of boards played.
  * @param notPlayed the number of boards that were not played.
  */
case class Card(totalMps: Rational, played: Int, notPlayed: Int, ps: Seq[BoardResult]) extends Ordered[Card] {

  def totalMpsAsString(top: Int): String = mpsAsString(scaledTotalMps, top)

  def compare(that: Card): Int = percentage.compare(that.percentage)

  def +(c: Card): Card = Card(totalMps + c.totalMps, played + c.played, notPlayed + c.notPlayed, ps ++ c.ps)

  lazy val toStringPercent: String = rationalToString(percentage) + "%" // CONSIDER use renderAsPercent

  private lazy val percentage: Rational = asPercent(totalMps, played)

  def scaledTotalMps: Rational = if (played > 0) totalMps * (played + notPlayed) / played else zero

  def render: String = {
    val rXb: Map[Int, BoardResult] = (for (p <- ps) yield p.board -> p).toMap
    val boards: Seq[Int] = rXb.keys.toSeq.sorted
    if (boards.nonEmpty) {
      val result = new StringBuilder(totalMps.toString())
      for (b <- 1 to boards.last) {
        val w = rXb.get(b) match {
          case Some(r) => s"$b: ${r.renderRo("")}"
          case None => ""
        }
        result.append("\t" + w)
      }
      result.toString
    }
    else "no boards played"
  }
}

/**
  * Companion object to `Card` class.
  */
object Card {

  /**
    * Apply method for Card that takes a sequence of tuples.
    *
    * @param roB a sequence of (BoardResult, Option[Rational]).
    * @return a Card.
    */
  def apply(roB: Seq[(BoardResult, Option[Rational])]): Card = {
    val (bs, ros) = (for ((b, ro) <- roB; r <- ro) yield (b.setMPs(r), ro)).unzip
    val rs: Seq[Rational] = ros.flatten
    // TODO fix this.
    //  We shouldn't have to rely on the played vs. not played mechanism
    //  (which requires DNP entries in order to work properly).
    Card(rs.sum, rs.size, roB.size - rs.size, bs)
  }
}

/**
  * This is the complete results for a particular direction of a section.
  *
  * @param isNS  (optional) Some(true) if this result is for N/S; Some(false) if for E/W; None for a single winner movement.
  * @param top   top on a board
  * @param cards a map of tuples containing total score and number of boards played, indexed by the pair number
  */
case class Result(tables: Int, isNS: Option[Boolean], top: Int, cards: Map[Int, Card]) {

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
    * The total matchpoints overall should be boards * sum{top}, i.e. n * (n - 1) * boards / 2
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
    val scaledMps = cards.map(_._2.scaledTotalMps)
    val matchpoints: Rational = scaledMps.sum
    val expected = Rational(tables * boards, 2)
    val result = matchpoints == expected
    val direction = isNS match {
      case Some(b) if b => "NS"
      case Some(_) => "EW"
      case None => "SW"
    }
    if (!result)
      System.err.println(s"Total fractional matchpoints for this $direction section result ($matchpoints) differ from what is expected for $boards boards and $tables tables ($boards * $tables / 2 = $expected)\n   (Note: this may not be a problem if there is a half-table)")
    result
  }

  private def getResultsForDirection(nameFunction: Int => String) = {

    case class Psi(len: Int, rank: Int, ps: Seq[Pos])
    object Psi {
      def empty: Psi = Psi(0, 0, Nil)
    }

    def resultDetails(s: (Pos, Int, String)): Output = {
      val ((pairNumber, card), rank, suffix) = s
      Output(s"""$rank$suffix\t$pairNumber\t${card.totalMpsAsString(top)}\t${card.toStringPercent}\t${nameFunction(pairNumber)}""").insertBreak
    }

    //    showCards() // NOTE comment this out for normal running.

    val keyFunction: Pos => Rational = _._2.scaledTotalMps
    val ps: Seq[Pos] = cards.toList.sortBy(keyFunction).reverse
    val psRm: Map[Rational, Seq[Pos]] = ps.groupBy[Rational](keyFunction)
    val psRs: Seq[(Rational, Seq[Pos])] = psRm.toSeq.sortBy(_._1).reverse
    val psis: Seq[Psi] = psRs.scanLeft(Psi.empty) {
      case (Psi(l, _, _), (_, ps)) => Psi(l + ps.length, l, ps)
    }
    val xPs: Seq[(Pos, Int, String)] = for {Psi(_, r, ps) <- psis; x = if (ps.size > 1) "=" else " "; p <- ps} yield (p, r + 1, x)
    val header = Output(s"Rank\tPair\tMPs\tPercent\tNames\n")
    Output.foldLeft(xPs)(header)(_ ++ resultDetails(_))
  }

  @unused
  private def showCards(): Unit = {
    println("pair\ttotal\tboards...")
    for {
      pair <- cards.keys.toSeq.sorted
      card = cards(pair)
    } println(s"$pair\t${card.render}")
    println(s"total\t${cards.values.map(_.totalMps).sum}")
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
    case Some(x) => s"""$ns\t$ew\t$result\t${Matchpoints.mpsAsString(x, top)}"""
    case _ => ""
  }

  private def factorACBL(idealTop: Int) = ro match {
    case None => None
    case Some(r) => Some(((r * top + half) * (idealTop + 1) / (top + 1) - half) / idealTop)
  }

  private def invert = ro map { r => -(r - 1) }

  def probableContract(board: Int): String = result.getProbableContract(Vulnerability(board)) getOrElse "CHECK!"

}

/**
  * Companion object to `Matchpoints`.
  */
object Matchpoints {

  /**
    * Method to render a Rational (r).
    *
    * @param r a Rational.
    * @return r rendered in five spaces.
    */
  //noinspection SpellCheckingInspection
  def rationalToString(r: Rational): String = r match {
    case Rational(x, Rational.bigOne) => f"$x%2d.00"
    case r: Rational if r.isInfinite => "infty"
    case _ => r.renderApproximate(5, Some(2))
  }

  /**
    * Method to get the matchpoints as a String.
    *
    * @param r   a Rational representing the fractional matchpoints.
    * @param top the top on a board.
    * @return a String.
    */
  def mpsAsString(r: Rational, top: Int): String = rationalToString(r * top)
}

/**
  * This is a traveler for a specific board (in a specific, unnamed, section).
  * We usually report boards either by travelers or pickup slips.
  * It is, however, possible to mix these up.
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
    result.append(s"Board: $board with $actualPlays plays\n")
    result.append(s"""NS pair\tEW pair\tNS score\tNS MPs\n""")
    for (m <- matchpoints) result.append(s"$m (probable contract: ${m.probableContract(board)})\n")
    output :+ result.toString
  }

  private def actualPlays = ps.count(_.result.played)

  private def calculateMatchpoints(idealTop: Int): Seq[Matchpoints] = {
    val result: Seq[Matchpoints] = for (p <- ps) yield Matchpoints(p.ns, p.ew, p.result, p.matchpoints(this), top).factorIfRequired(idealTop)
    val total = result.flatMap(_.getMatchpoints(true)).sum
    val expectedTotal = Rational((idealTop + 1) / 2)

    if (total != expectedTotal)
      System.err.println(s"Board $board: has incorrect total matchpoints: $total (expected: $expectedTotal)")
    result
  }
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
case class BoardResult(board: Int, result: PlayResult, ro: Option[Rational] = None) {
  override def toString: String = s"$board: ${renderRo(" for ")}$result"

  def renderRo(suffix: String): String = (ro map (_.renderAsPercent(2) + suffix)).getOrElse("")

  def setMPs(r: Rational): BoardResult = copy(ro = Some(r))
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
  // CONSIDER moving this method into Traveler
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

  /**
    * Method to compare scores for matchpointing.
    *
    * CONSIDER rewriting this (perhaps matching on a tuple of play results) and maybe moving it into `PlayResult`.
    *
    * @param x the `PlayResult` to be compared with `this.result`.
    * @return an `Option[Boolean]`.
    */
  def compare(x: PlayResult): Option[Int] = result match {
    case PlayResult(Right(y)) => x match {
      case PlayResult(Right(z)) => Some(Integer.compare(z, y) + 1)
      case _ => None
    }
    case _ => None
  }


  def playResult(n: Int, dirNs: Boolean): Option[PlayResult] = if (dirNs) Play.conditional(result)(n == ns) else Play.conditional(result.invert)(n == ew)

  /**
    * Calculate the matchpoints for this Play in the context of the given Traveler (t)
    *
    * @param t the traveler (i.e., the board) on which we find this Play.
    * @return an optional Rational.
    */
  def matchpoints(t: Traveler): Option[Rational] = result.matchpoints(t.matchpoint(this))

  /**
    * Here we check if the result is one of the common possible results.
    *
    * @param board the board number.
    */
  def checkScore(board: Int): Boolean = {
    val z = result.checkScore(Vulnerability(board))
    if (!z) println(s"Board $board needs check on result $result")
    z
  }
}

/**
  * Companion object to Play class.
  */
object Play {
  def apply(ns: Try[Int], ew: Try[Int], result: PlayResult): Play = {
    val z = for (x <- ns; y <- ew) yield Play(x, y, result)
    z.recover { case x => System.err.println(s"Exception: $x"); Play(0, 0, Checker.error("no match")) }.get
  }

  /**
    * Method to yield an optional value of `X` based on a condition.
    *
    * CONSIDER moving this to a utility object
    *
    * @param x the `X` value to be yielded (call-by-name).
    * @param b the condition.
    * @tparam X the underlying type of the result.
    * @return `Some(x)` if `b` is true, otherwise `None`.
    */
  def conditional[X](x: => X)(b: Boolean): Option[X] = if (b) Some(x) else None

}
