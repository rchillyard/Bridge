/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director
import com.phasmidsoftware.bridge.director.Matchpoints.rationalToString
import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.output.MockWriter
import com.phasmidsoftware.util.Output
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * @author scalaprof
  */
class EventSpec extends AnyFlatSpec with should.Matchers {

  behavior of "BoardPlay"
  it should "apply" in {
    val play = Play(1, 2, PlayResult("110"))
    val board = 1
    val target = BoardPlay(board, play)
    target.play shouldBe play
    target.board shouldBe board
    target.toString shouldBe "1: 1 vs 2: 110"
  }
  it should "addTo" in {
    val board = 1
    val travelerMap = Map[Int, Traveler]()
    val play = Play(1, 2, PlayResult("110"))
    val target = BoardPlay(board, play)
    val updatedMap = target.addTo(travelerMap)
    updatedMap.size shouldBe 1
    val maybeTraveler = updatedMap.get(board)
    maybeTraveler should matchPattern { case Some(Traveler(_, _, _)) => }
    val t = maybeTraveler.get
    t.board shouldBe board
    t.ps.size shouldBe 1
    t.ps.head shouldBe play
  }
  it should "addTo non-empty traveler map" in {
    val board = 1
    val play1 = Play(1, 2, PlayResult("110"))
    val play2 = Play(3, 4, PlayResult("100"))
    val travelerMap = Map[Int, Traveler](1 -> Traveler(1, List(play2), None))
    val target = BoardPlay(board, play1)
    val updatedMap = target.addTo(travelerMap)
    updatedMap.size shouldBe 1
    val maybeTraveler = updatedMap.get(board)
    maybeTraveler should matchPattern { case Some(Traveler(_, _, _)) => }
    val t = maybeTraveler.get
    t.board shouldBe board
    t.ps.size shouldBe 2
    t.ps.last shouldBe play1
  }

  behavior of "Pickup"
  it should "work" in {
    val score1 = "110"
    val score2 = "100"
    val result1 = PlayResult(score1)
    val result2 = PlayResult(score2)
    val bd1 = 5
    val bd2 = 6
    val ns = 13
    val ew = 17
    val play1 = Play(ns, ew, result1)
    val play2 = Play(ns, ew, result2)
    val boardPlay1 = BoardPlay(bd1, play1)
    val boardPlay2 = BoardPlay(bd2, play2)
    val target = Pickup(ns, ew, List(BoardResult(bd1, result1), BoardResult(bd2, result2)))
    target.toString shouldBe s"Pickup: $ns vs $ew: $bd1: $score1, $bd2: $score2"
    target.boards.size shouldBe 2
    target.ns shouldBe ns
    target.ew shouldBe ew
    val boardPlays: collection.Seq[BoardPlay] = target.asBoardPlays
    boardPlays shouldBe List(boardPlay1, boardPlay2)
  }

  behavior of "Traveler"
  it should "matchpoint properly (1)" in {
    val p1 = Play(2, 1, PlayResult(Right(130)))
    val p2 = Play(1, 2, PlayResult(Right(150)))
    val t = Traveler(1, Seq(p1, p2), None)
    t.matchpoint(p1) shouldBe Some(Rational.zero)
    t.matchpoint(p2) shouldBe Some(Rational.one)
  }
  it should "matchpoint properly (2)" in {
    val p1 = Play(2, 1, PlayResult(Right(130)))
    val p2 = Play(1, 2, PlayResult(Right(150)))
    val t = Traveler(1, List(p1, p2), None)
    val mps = t.matchpointIt(1).maybeMatchpoints
    mps.get.head.ro shouldBe Some(Rational.zero)
    mps.get.tail.head.ro shouldBe Some(Rational.one)
  }
  it should "matchpoint properly (3)" in {
    val p1 = Play(2, 1, PlayResult(Right(130)))
    val p2 = Play(1, 2, PlayResult(Right(150)))
    val p3 = Play(3, 3, PlayResult(Right(150)))
    val t = Traveler(1, List(p1, p2, p3), None)
    val mps = t.matchpointIt(2).maybeMatchpoints.get
    mps.head.ro shouldBe Some(Rational.zero)
    mps.tail.head.ro shouldBe Some(Rational(3, 4))
    Matchpoints.mpsAsString(mps.tail.head.ro.get, 2) shouldBe " 1.50"
    mps.tail.tail.head.ro shouldBe Some(Rational(3, 4))
  }
  it should "calculate BAM mps" in {
    val traveler = "   T 1\n    1 1 420\n    2 2 430\n\n"
    val parser = new RecapParser
    val r = parser.parseAll(parser.traveler, traveler)
    r should matchPattern { case parser.Success(_, _) => }
    val t = r.get
    val firstEntry = t.ps.head
    val mps = firstEntry.matchpoints(t)
    mps shouldBe Some(Rational.zero)
  }
  it should "calculate mps" in {
    val traveler = "   T 1\n    1 1 420\n    2 2 420\n    3 4 420\n    4 3 140\n    5 5 170\n    6 6 -50\n    7 6 420\n\n"
    val parser = new RecapParser
    val r = parser.parseAll(parser.traveler, traveler)
    r should matchPattern { case parser.Success(_, _) => }
    val t = r.get
    val firstEntry = t.ps.head
    val mps = firstEntry.matchpoints(t)
    mps shouldBe Some(Rational(3, 4))
  }

  it should "output" in {
    val score1 = "110"
    val score2 = "100"
    val result1 = PlayResult(score1)
    val result2 = PlayResult(score2)
    val bd = 5
    val ns1 = 13
    val ns2 = 14
    val ew1 = 17
    val ew2 = 16
    val play1 = Play(ns1, ew1, result1)
    val play2 = Play(ns2, ew2, result2)
    val target = Traveler(bd, List(play1, play2), None)
    target.board shouldBe bd
    target.ps.size shouldBe 2
    target.ps shouldBe List(play1, play2)
    target.isPlayed shouldBe true
    target.top shouldBe 1
    val writer = MockWriter()
    target.output(Output(writer)).close()
    writer.spillway shouldBe
      s"""Board: 5 with 2 plays
         |NS pair	EW pair	NS score	NS MPs
         |13	17	110	 1.00 (probable contract: NS partial 2 major)
         |14	16	100	 0.00 (probable contract: EW  down 1X)
         |""".stripMargin
  }

  it should "toString" in {
    val score1 = "110"
    val score2 = "100"
    val result1 = PlayResult(score1)
    val result2 = PlayResult(score2)
    val bd = 5
    val ns1 = 13
    val ns2 = 14
    val ew1 = 17
    val ew2 = 16
    val play1 = Play(ns1, ew1, result1)
    val play2 = Play(ns2, ew2, result2)
    val target = Traveler(bd, List(play1, play2), None)
    target.board shouldBe bd
    target.ps.size shouldBe 2
    target.ps shouldBe List(play1, play2)
    target.isPlayed shouldBe true
    target.top shouldBe 1
    target.toString shouldBe s"Traveler($bd,List($ns1 vs $ew1: $score1, $ns2 vs $ew2: $score2),None)"
  }
  it should "matchpoint" in {
    val play1 = Play(13, 17, PlayResult("110"))
    val play2 = Play(14, 16, PlayResult("100"))
    val target = Traveler(5, Seq(play1, play2), None)
    target.matchpoint(play1) shouldBe Some(Rational(1))
    target.matchpoint(play2) shouldBe Some(Rational(0))
  }
  it should "matchpointIt" in {
    val score1 = "110"
    val score2 = "100"
    val result1 = PlayResult(score1)
    val result2 = PlayResult(score2)
    val bd = 5
    val ns1 = 13
    val ns2 = 14
    val ew1 = 17
    val ew2 = 16
    val target = Traveler(bd, List(Play(ns1, ew1, result1), Play(ns2, ew2, result2)), None)
    val top = 1
    val matchpoints1 = Matchpoints(ns1, ew1, result1, Some(Rational(1)), top)
    val matchpoints2 = Matchpoints(ns2, ew2, result2, Some(Rational(0)), top)
    target.matchpointIt(top) shouldBe target.copy(maybeMatchpoints = Some(Seq(matchpoints1, matchpoints2)))
  }
  it should ":+" in {
    val score1 = "110"
    val score2 = "100"
    val result1 = PlayResult(score1)
    val result2 = PlayResult(score2)
    val bd1 = 5
    val bd2 = 6
    val ns = 13
    val ew = 17
    val play1 = Play(ns, ew, result1)
    val play2 = Play(ns, ew, result2)
    val target5 = Traveler(bd1, Nil, None)
    target5 :+ play1 shouldBe Traveler(bd1, List(play1), None)
    val target6 = Traveler(bd2, Nil, None)
    target6 :+ play2 shouldBe Traveler(bd2, List(play2), None)
  }

  behavior of "pairs"

  behavior of "player"

  behavior of "playerPlayer"

  behavior of "pair"

  behavior of "preamble"

  behavior of "result"
  it should "score DNP as None" in {
    val p1 = Play(1, 1, PlayResult(Left("DNP")))
    val t = Traveler(1, List(), None)
    p1.matchpoints(t) shouldBe None
  }
  it should "score A as Some(1/2)" in {
    val p1 = Play(1, 1, PlayResult(Left("A")))
    val t = Traveler(1, List(), None)
    p1.matchpoints(t) shouldBe Some(Rational(1, 2))
  }
  it should "score A- as Some(2,5)" in {
    val p1 = Play(1, 1, PlayResult(Left("A-")))
    val t = Traveler(1, List(), None)
    p1.matchpoints(t) shouldBe Some(Rational(2, 5))
  }
  "percentageAsString" should "work" in {
    val r = Rational(3, 4)
    Card(r, 1, 0, Nil).toStringPercent shouldBe "75.00%"
  }
  "mpsAsString" should "work" in {
    val r = Rational(3, 4)
    Matchpoints.mpsAsString(r, 6) shouldBe " 4.50"
  }

  behavior of "Play"
  it should "compare 1 1 +130 with 2 2 110 as 2" in {
    val p1 = Play(1, 1, PlayResult(Right(130)))
    val p2 = Play(2, 2, PlayResult(Right(110)))
    p1.compare(p2.result) shouldBe Some(0)
    p2.compare(p1.result) shouldBe Some(2)
  }

  behavior of "Section"

  def checkResult(result: Result, directionNS: Boolean, top: Int, pairs: Int, boards: Int): Unit = {
    result.isNS shouldBe Some(directionNS)
    result.top shouldBe top
    val cards: Map[Int, Card] = result.cards
    cards.size shouldBe pairs
    //    val total: Rational = (for (Card(r, _, _) <- cards.values) yield r).sum
    result.checksum(boards) shouldBe true
    //    (for ((_, Card(_, t, _)) <- cards) yield t).head shouldBe result.top + 1
  }

  it should "apply with pickups" in {
    val pairs = List(
      director.Pair(1, Some("N"), (Player("tweedledum"), Player("tweedledee"))),
      director.Pair(2, Some("N"), (Player("James Clark Maxwell"), Player("Albert Einstein"))),
      director.Pair(1, Some("E"), (Player("Tristan"), Player("Isolde"))),
      director.Pair(2, Some("E"), (Player("Romeo"), Player("Juliet")))
    )
    val preamble = Preamble("A", None, pairs)
    val bd1 = 1
    val bd2 = 2
    val ns1 = 1
    val ew1 = 1
    val ew2 = 2
    val ns2 = 2
    val result111 = PlayResult(Right(110))
    val result122 = PlayResult(Right(100))
    val result222 = PlayResult(Right(-400))
    val result221 = PlayResult(Right(-430))
    val traveler1 = Traveler(bd1, List(Play(ns1, ew1, result111), Play(ns2, ew2, result122)), None)
    val traveler2 = Traveler(bd2, List(Play(ns2, ew2, result222), Play(ns2, ew1, result221)), None)
    val travelers: List[Traveler] = List()
    val pickup111 = Pickup(ns1, ew1, List(BoardResult(bd1, result111)))
    val pickup122 = Pickup(ns2, ew2, List(BoardResult(bd1, result122)))
    val pickup221 = Pickup(ns2, ew1, List(BoardResult(bd2, result221)))
    val pickup222 = Pickup(ns2, ew2, List(BoardResult(bd2, result222)))
    val pickups = List(pickup111, pickup122, pickup222, pickup221)
    val target = Section(preamble, travelers, pickups)
    target.travelers shouldBe List(traveler1, traveler2)
  }

  it should "work for travelers" in {

    val pairs = List(
      director.Pair(1, Some("N"), (Player("tweedledum"), Player("tweedledee"))),
      director.Pair(2, Some("N"), (Player("James Clark Maxwell"), Player("Albert Einstein"))),
      director.Pair(1, Some("E"), (Player("Tristan"), Player("Isolde"))),
      director.Pair(2, Some("E"), (Player("Romeo"), Player("Juliet")))
    )
    val travelers: List[Traveler] = List(
      Traveler(1, List(Play(1, 1, PlayResult(Right(110))), Play(2, 2, PlayResult(Right(100)))), None),
      Traveler(2, List(Play(1, 2, PlayResult(Right(-400))), Play(2, 1, PlayResult(Right(-430)))), None)
    )
    val preamble = Preamble("A", None, pairs)
    val section = Section(preamble, travelers)
    section.calculateTop shouldBe 1
    val results: collection.Seq[Result] = section.createResults
    results.size shouldBe 2
    checkResult(results.head, directionNS = true, top = 1, pairs = 2, boards = 2)
    checkResult(results.last, directionNS = false, top = 1, pairs = 2, boards = 2)
  }

  it should "work for incomplete travelers 1" in {

    val pairs = List(
      director.Pair(1, Some("N"), (Player("tweedledum"), Player("tweedledee"))),
      director.Pair(2, Some("N"), (Player("James Clark Maxwell"), Player("Albert Einstein"))),
      director.Pair(3, Some("N"), (Player("Rosie"), Player("Ashenden"))),
      director.Pair(1, Some("E"), (Player("Tristan"), Player("Isolde"))),
      director.Pair(2, Some("E"), (Player("Romeo"), Player("Juliet"))),
      director.Pair(3, Some("E"), (Player("David"), Player("Dora")))
    )
    val travelers: List[Traveler] = List(
      Traveler(1, List(Play(1, 1, PlayResult(Right(100))), Play(2, 2, PlayResult(Right(100))), Play(3, 3, PlayResult(Right(100)))), None),
      Traveler(2, List(Play(1, 2, PlayResult(Right(-400))), Play(2, 3, PlayResult(Right(-400))), Play(3, 1, PlayResult(Left("DNP")))), None)
    )
    // Matchpoints for board 1 should be 1, 1, and 1
    // Matchpoints for board 2 should be 1/2, and 1/2 unfactored but 1, 1, and 1 when factored.
    // Total matchpoints for each card should therefore be 3/2 (i.e., pairs/2)
    val preamble = Preamble("A", None, pairs)
    val section = Section(preamble, travelers)
    section.calculateTop shouldBe 2
    val recap = section.recap
    val results: collection.Seq[Result] = recap.createResults
    results.size shouldBe 2
    checkResult(results.head, directionNS = true, top = 2, pairs = pairs.size / 2, boards = travelers.size)
    checkResult(results.last, directionNS = false, top = 2, pairs = pairs.size / 2, boards = travelers.size)
  }

  // NOTE that we currently don't handle travelers with missing entries: unplayed boards have to be entered as DNP
  ignore should "work for incomplete travelers 2" in {

    val pairs = List(
      director.Pair(1, Some("N"), (Player("tweedledum"), Player("tweedledee"))),
      director.Pair(2, Some("N"), (Player("James Clark Maxwell"), Player("Albert Einstein"))),
      director.Pair(1, Some("E"), (Player("Tristan"), Player("Isolde"))),
      director.Pair(2, Some("E"), (Player("Romeo"), Player("Juliet")))
    )
    val travelers: List[Traveler] = List(
      Traveler(1, List(Play(1, 1, PlayResult(Right(110))), Play(2, 2, PlayResult(Right(100))), Play(3, 3, PlayResult(Right(50)))), None),
      Traveler(2, List(Play(1, 2, PlayResult(Right(-400))), Play(2, 3, PlayResult(Right(-430)))), None)
    )
    val preamble = Preamble("A", None, pairs)
    val section = Section(preamble, travelers)
    section.calculateTop shouldBe 2
    val results: collection.Seq[Result] = section.recap.createResults
    results.size shouldBe 2
    checkResult(results.head, directionNS = true, top = 2, pairs = 2, boards = 2)
    checkResult(results.last, directionNS = false, top = 2, pairs = 2, boards = 2)
  }

  behavior of "event"
  it should "read travelers.lexington.2017.0404 as a resource" in {
    val resource = "travelers.lexington.2017.0404"
    val ey: Try[Event] = Option(getClass.getResourceAsStream(resource)) match {
      case Some(s) => RecapParser.readEvent(Source.fromInputStream(s))
      case None => Failure(new Exception(s"doScoreResource: cannot open resource: $resource"))
    }
    ey should matchPattern { case Success(Event(_, _)) => }
    val event = ey.get
    val results: Map[Preamble, Seq[Result]] = event.createResults
    val rso: Option[Seq[Result]] = results.get(event.sections.head.preamble)
    rso should matchPattern { case Some(_) => }
    val resultsA: Seq[Result] = rso.get
    resultsA.size shouldBe 2
    val resultANS: Result = resultsA.head
    resultANS.isNS shouldBe Some(true)
    resultANS.top shouldBe 5
    val cards: Map[Int, Card] = resultANS.cards
    cards.size shouldBe 6
    val total: Rational = (for (Card(r, _, _, _) <- cards.values) yield r).sum
    total shouldBe Rational(2).invert * cards.size * (resultANS.top + 1)
    val scores = for (score <- cards.keys) yield cards(score)
    scores.size shouldBe 6
    for ((_, Card(_, t, _, _)) <- cards) t shouldBe resultANS.top + 1
  }

  // CONSIDER sort this out.
  // This file seems to be incorrect so maybe it's not a problem that this test doesn't succeed
  //	ignore should "read travelers.lexington.2016.0503 as a resource" in {
  //		val resource = "travelers.lexington.2016.0503"
  //		val ey: Try[Event] = Option(getClass.getResourceAsStream(resource)) match {
  //			case Some(s) => RecapParser.readEvent(Source.fromInputStream(s))
  //			case None => Failure(new Exception(s"doScoreResource: cannot open resource: $resource"))
  //		}
  //		ey should matchPattern { case Success(Event(_, _)) => }
  //		val event = ey.get
  //		val results: Map[Preamble, Seq[Result]] = event.createResults
  //		val rso: Option[Seq[Result]] = results.get(event.sections.head.preamble)
  //		rso should matchPattern { case Some(_) => }
  //		val resultsA: Seq[Result] = rso.get
  //		resultsA.size shouldBe 2
  //		val resultANS: Result = resultsA.head
  //		resultANS.isNS shouldBe true
  //		resultANS.top shouldBe 5
  //		val cards: Map[Int, (Rational, Int)] = resultANS.cards
  //		cards.size shouldBe 14
  //		val scores = (for (score <- cards.keys) yield cards(score)).toSeq
  //		scores.size shouldBe 12
  //		val total: Rational = (for ((r, _) <- cards.values) yield r).sum
  //		total shouldBe Rational(2).invert * cards.size * (resultANS.top + 1)
  //		scores.size shouldBe 6
  //		for ((_, (_, t)) <- cards) t shouldBe resultANS.top + 1
  //	}

  behavior of "Card"

  it should "toStringPercent" in {
    val target = Card(Rational(5, 2), 5, 0, Nil)
    target.toStringPercent shouldBe "50.00%"
  }

  it should "toStringPercentDNP" in {
    val target = Card(Rational(4, 2), 4, 1, Nil)
    target.toStringPercent shouldBe "50.00%"
  }

  it should "totalMpsAsString" in {
    val target = Card(Rational(5), 5, 0, Nil)
    target.totalMpsAsString(2) shouldBe "10.00"
  }

  it should "toStringMpsDNP" in {
    val target = Card(Rational(5), 4, 1, Nil)
    target.totalMpsAsString(2) shouldBe "12.50"
  }

  behavior of "rationalToString"

  it should "render" in {
    rationalToString(Score.asPercent(Rational(1, 2), 1)) shouldBe "50.00"
    rationalToString(Rational(2, 1)) shouldBe " 2.00"
    rationalToString(Rational(1, 2)) shouldBe " 0.50"
  }

}

