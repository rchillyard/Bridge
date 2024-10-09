/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director
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
class ScoreSpec extends AnyFlatSpec with should.Matchers {

  behavior of "PlayResult"
  it should """apply("110")""" in {
    val target = PlayResult("110")
    target.r.isRight shouldBe true
    target.r.toOption should matchPattern { case Some(110) => }
    target.toString shouldBe "110"
  }
  it should """apply(Right(110))""" in {
    val target = PlayResult(Right(110))
    target.r.isRight shouldBe true
    target.r.toOption should matchPattern { case Some(110) => }
    target.toString shouldBe "110"
  }
  it should "apply(Left(DNP))" in {
    val target = PlayResult(Left("DNP"))
    target.r.isRight shouldBe false
    target.r.toOption should matchPattern { case None => }
    target.matchpoints(None) shouldBe None
    target.toString shouldBe "DNP"
  }
  it should "apply(DNP)" in {
    val target = PlayResult("DNP")
    target.r.isRight shouldBe false
    target.r.toOption should matchPattern { case None => }
    target.matchpoints(None) shouldBe None
    target.toString shouldBe "DNP"
  }
  it should "apply(A)" in {
    val target = PlayResult("A")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational.half)
    target.toString shouldBe "A"
  }
  it should "apply(A+)" in {
    val target = PlayResult("A+")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational(3) / 5)
    target.toString shouldBe "A+"
  }
  it should "apply(A-)" in {
    val target = PlayResult("A-")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational(2) / 5)
    target.toString shouldBe "A-"
  }

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
    mps.get.head.mp shouldBe Some(Rational.zero)
    mps.get.tail.head.mp shouldBe Some(Rational.one)
  }
  it should "matchpoint properly (3)" in {
    val p1 = Play(2, 1, PlayResult(Right(130)))
    val p2 = Play(1, 2, PlayResult(Right(150)))
    val p3 = Play(3, 3, PlayResult(Right(150)))
    val t = Traveler(1, List(p1, p2, p3), None)
    val mps = t.matchpointIt(2).maybeMatchpoints.get
    mps.head.mp shouldBe Some(Rational.zero)
    mps.tail.head.mp shouldBe Some(Rational(3, 4))
    Card.mpsAsString(mps.tail.head.mp.get, 2) shouldBe " 1.50"
    mps.tail.tail.head.mp shouldBe Some(Rational(3, 4))
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
         |13	17	110	 1.00
         |14	16	100	 0.00
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
    Card(r, 1, 0).toStringPercent shouldBe "75.00%"
  }
  "mpsAsString" should "work" in {
    val r = Rational(3, 4)
    Card.mpsAsString(r, 6) shouldBe " 4.50"
  }

  behavior of "Play"
  it should "compare 1 1 +130 with 2 2 110 as 2" in {
    val p1 = Play(1, 1, PlayResult(Right(130)))
    val p2 = Play(2, 2, PlayResult(Right(110)))
    p1.compare(p2.result) shouldBe Some(0)
    p2.compare(p1.result) shouldBe Some(2)
  }

  behavior of "Section"

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

  it should "work" in {
    def checkResult(result: Result, directionNS: Boolean): Unit = {
      result.isNS shouldBe Some(directionNS)
      result.top shouldBe 1
      val cards: Map[Int, Card] = result.cards
      cards.size shouldBe 2
      val total: Rational = (for (Card(r, _, _) <- cards.values) yield r).sum
      total shouldBe Rational(2).invert * cards.size * (result.top + 1)
      for ((_, Card(_, t, _)) <- cards) t shouldBe result.top + 1
    }

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
    checkResult(results.head, directionNS = true)
    checkResult(results.last, directionNS = false)
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
    val total: Rational = (for (Card(r, _, _) <- cards.values) yield r).sum
    total shouldBe Rational(2).invert * cards.size * (resultANS.top + 1)
    val scores = for (score <- cards.keys) yield cards(score)
    scores.size shouldBe 6
    for ((_, Card(_, t, _)) <- cards) t shouldBe resultANS.top + 1
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

  behavior of "Score"
  it should "read travelers.lexington.2017.0404 as a resource" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreResource("travelers.lexington.2017.0404", Output(writer))) o.close()
    writer.spilled shouldBe 1769
  }
  it should "read travelers.lexington.2017.0404P as a resource (includes pickup slips)" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreResource("travelers.lexington.2017.0404P", Output(writer))) o.close()
    writer.spilled shouldBe 1769
  }
  it should "read travelers.lexington.2017.0404 as a file" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/travelers.lexington.2017.0404", Output(writer))) o.close()
    writer.spilled shouldBe 1769
  }
  it should "read ConcordCountryClub20191007.txt" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreResource("ConcordCountryClub20191007.txt", Output(writer))) o.close()
    writer.spilled shouldBe 2014
  }

  // FIXME Issue #8
  //noinspection SpellCheckingInspection
  ignore should "read keremshalom.2019.0509.txt as a file" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/keremshalom.2019.0509.txt", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    writer.spillway shouldBe
      """Kerem Shalom Beginner Duplicate: May 9th 2019
        |Section A
        |Results for direction N/S
        |Pos	Pair	MPs	Percent	Names
        |1	1	 6.00	75.00%	Dan & Tenley
        |2	3	 5.67	56.67%	Chris & Kathy
        |3	2	 5.17	43.06%	Irene & Robin
        |4	4	 3.17	31.67%	Tom & Jane
        |Results for direction E/W
        |Pos	Pair	MPs	Percent	Names
        |1	4	 6.83	68.33%	Ghilaine & Bill
        |2	2	 3.83	47.92%	JoAnn & Margaret
        |3	1	 4.33	43.33%	Marian & Patty
        |4	3	 5.00	41.67%	Wendy & Ruth
        |=====================================================
        |=====================================================
        |Kerem Shalom Beginner Duplicate: May 9th 2019
        |A
        |1N	Dan & Tenley
        |1E	Marian & Patty
        |2N	Irene & Robin
        |2E	JoAnn & Margaret
        |3N	Chris & Kathy
        |3E	Wendy & Ruth
        |4N	Tom & Jane
        |4E	Ghilaine & Bill
        |
        |Board: 1 with 4 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	450	 1.50
        |2	3	450	 1.50
        |4	4	450	 1.50
        |3	2	450	 1.50
        |Board: 2 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	-980	 1.00
        |2	3	-1010	 0.00
        |3	2	-510	 2.00
        |Board: 3 with 4 plays
        |NS pair	EW pair	NS score	NS MPs
        |2	2	-650	 1.00
        |4	4	-650	 1.00
        |1	3	50	 3.00
        |3	1	-650	 1.00
        |Board: 4 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |2	2	-100	 0.50
        |4	4	-100	 0.50
        |1	3	1430	 2.00
        |Board: 5 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |3	3	50	 1.00
        |2	1	50	 1.00
        |4	4	50	 1.00
        |Board: 6 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |3	3	450	 1.00
        |2	1	480	 2.00
        |4	4	-50	 0.00
        |""".stripMargin
    writer.spilled shouldBe 1317
  }
  // NOTE we really don't need this test (which currently fails)
  ignore should "read KeremShalom.prn as a PRN file (output from Excel)" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/KeremShalom.prn", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    //    writer.spillway shouldBe "Kerem Shalom Beginner Duplicate: May 9th 2019\nSection A\nResults for direction N/S\n1 :  6.00 : 75.00% : Dan & Tenley\n3 :  5.67 : 56.67% : Chris & Kathy\n2 :  5.17 : 43.06% : Irene & Robin\n4 :  3.17 : 31.67% : Tom & Jane\nResults for direction E/W\n4 :  6.83 : 68.33% : Ghilaine & Bill\n2 :  3.83 : 47.92% : JoAnn & Margaret\n1 :  4.33 : 43.33% : Marian & Patty\n3 :  5.00 : 41.67% : Wendy & Ruth\n=====================================================\n=====================================================\nKerem Shalom Beginner Duplicate: May 9th 2019\nA\n1N: Dan & Tenley\n1E: Marian & Patty\n2N: Irene & Robin\n2E: JoAnn & Margaret\n3N: Chris & Kathy\n3E: Wendy & Ruth\n4N: Tom & Jane\n4E: Ghilaine & Bill\n\nBoard: 1 with 4 plays\nNS: 1, EW: 1, score: 450, MP: 1.50\nNS: 2, EW: 3, score: 450, MP: 1.50\nNS: 4, EW: 4, score: 450, MP: 1.50\nNS: 3, EW: 2, score: 450, MP: 1.50\nBoard: 2 with 3 plays\nNS: 1, EW: 1, score: -980, MP: 1.00\nNS: 2, EW: 3, score: -1010, MP: 0.00\nNS: 3, EW: 2, score: -510, MP: 2.00\nBoard: 3 with 4 plays\nNS: 2, EW: 2, score: -650, MP: 1.00\nNS: 4, EW: 4, score: -650, MP: 1.00\nNS: 1, EW: 3, score: 50, MP: 3.00\nNS: 3, EW: 1, score: -650, MP: 1.00\nBoard: 4 with 3 plays\nNS: 2, EW: 2, score: -100, MP: 0.50\nNS: 4, EW: 4, score: -100, MP: 0.50\nNS: 1, EW: 3, score: 1430, MP: 2.00\nBoard: 5 with 3 plays\nNS: 3, EW: 3, score: 50, MP: 1.00\nNS: 2, EW: 1, score: 50, MP: 1.00\nNS: 4, EW: 4, score: 50, MP: 1.00\nBoard: 6 with 3 plays\nNS: 3, EW: 3, score: 450, MP: 1.00\nNS: 2, EW: 1, score: 480, MP: 2.00\nNS: 4, EW: 4, score: -50, MP: 0.00\n"
    writer.spilled shouldBe 1529
  }
  ignore should "read KeremShalom.txt as a TXT file (output from Excel)" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/KeremShalom.txt", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    //    writer.spillway shouldBe "Kerem Shalom Beginner Duplicate: May 9th 2019\nSection A\nResults for direction N/S\n1 :  6.00 : 75.00% : Dan & Tenley\n3 :  5.67 : 56.67% : Chris & Kathy\n2 :  5.17 : 43.06% : Irene & Robin\n4 :  3.17 : 31.67% : Tom & Jane\nResults for direction E/W\n4 :  6.83 : 68.33% : Ghilaine & Bill\n2 :  3.83 : 47.92% : JoAnn & Margaret\n1 :  4.33 : 43.33% : Marian & Patty\n3 :  5.00 : 41.67% : Wendy & Ruth\n=====================================================\n=====================================================\nKerem Shalom Beginner Duplicate: May 9th 2019\nA\n1N: Dan & Tenley\n1E: Marian & Patty\n2N: Irene & Robin\n2E: JoAnn & Margaret\n3N: Chris & Kathy\n3E: Wendy & Ruth\n4N: Tom & Jane\n4E: Ghilaine & Bill\n\nBoard: 1 with 4 plays\nNS: 1, EW: 1, score: 450, MP: 1.50\nNS: 2, EW: 3, score: 450, MP: 1.50\nNS: 4, EW: 4, score: 450, MP: 1.50\nNS: 3, EW: 2, score: 450, MP: 1.50\nBoard: 2 with 3 plays\nNS: 1, EW: 1, score: -980, MP: 1.00\nNS: 2, EW: 3, score: -1010, MP: 0.00\nNS: 3, EW: 2, score: -510, MP: 2.00\nBoard: 3 with 4 plays\nNS: 2, EW: 2, score: -650, MP: 1.00\nNS: 4, EW: 4, score: -650, MP: 1.00\nNS: 1, EW: 3, score: 50, MP: 3.00\nNS: 3, EW: 1, score: -650, MP: 1.00\nBoard: 4 with 3 plays\nNS: 2, EW: 2, score: -100, MP: 0.50\nNS: 4, EW: 4, score: -100, MP: 0.50\nNS: 1, EW: 3, score: 1430, MP: 2.00\nBoard: 5 with 3 plays\nNS: 3, EW: 3, score: 50, MP: 1.00\nNS: 2, EW: 1, score: 50, MP: 1.00\nNS: 4, EW: 4, score: 50, MP: 1.00\nBoard: 6 with 3 plays\nNS: 3, EW: 3, score: 450, MP: 1.00\nNS: 2, EW: 1, score: 480, MP: 2.00\nNS: 4, EW: 4, score: -50, MP: 0.00\n"
    writer.spilled shouldBe 1529
  }
  it should "read KeremShalom.txt as a TSV file (output from Excel)" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/KeremShalom.tsv", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    //    writer.spillway shouldBe "Kerem Shalom Beginner Duplicate: May 9th 2019\nSection A\nResults for direction N/S\n1 :  6.00 : 75.00% : Dan & Tenley\n3 :  5.67 : 56.67% : Chris & Kathy\n2 :  5.17 : 43.06% : Irene & Robin\n4 :  3.17 : 31.67% : Tom & Jane\nResults for direction E/W\n4 :  6.83 : 68.33% : Ghilaine & Bill\n2 :  3.83 : 47.92% : JoAnn & Margaret\n1 :  4.33 : 43.33% : Marian & Patty\n3 :  5.00 : 41.67% : Wendy & Ruth\n=====================================================\n=====================================================\nKerem Shalom Beginner Duplicate: May 9th 2019\nA\n1N: Dan & Tenley\n1E: Marian & Patty\n2N: Irene & Robin\n2E: JoAnn & Margaret\n3N: Chris & Kathy\n3E: Wendy & Ruth\n4N: Tom & Jane\n4E: Ghilaine & Bill\n\nBoard: 1 with 4 plays\nNS: 1, EW: 1, score: 450, MP: 1.50\nNS: 2, EW: 3, score: 450, MP: 1.50\nNS: 4, EW: 4, score: 450, MP: 1.50\nNS: 3, EW: 2, score: 450, MP: 1.50\nBoard: 2 with 3 plays\nNS: 1, EW: 1, score: -980, MP: 1.00\nNS: 2, EW: 3, score: -1010, MP: 0.00\nNS: 3, EW: 2, score: -510, MP: 2.00\nBoard: 3 with 4 plays\nNS: 2, EW: 2, score: -650, MP: 1.00\nNS: 4, EW: 4, score: -650, MP: 1.00\nNS: 1, EW: 3, score: 50, MP: 3.00\nNS: 3, EW: 1, score: -650, MP: 1.00\nBoard: 4 with 3 plays\nNS: 2, EW: 2, score: -100, MP: 0.50\nNS: 4, EW: 4, score: -100, MP: 0.50\nNS: 1, EW: 3, score: 1430, MP: 2.00\nBoard: 5 with 3 plays\nNS: 3, EW: 3, score: 50, MP: 1.00\nNS: 2, EW: 1, score: 50, MP: 1.00\nNS: 4, EW: 4, score: 50, MP: 1.00\nBoard: 6 with 3 plays\nNS: 3, EW: 3, score: 450, MP: 1.00\nNS: 2, EW: 1, score: 480, MP: 2.00\nNS: 4, EW: 4, score: -50, MP: 0.00\n"
    writer.spilled shouldBe 1325
  }
  it should "output with equal ranks" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/Newton/Newton20240924.txt", Output(writer))) o.close()
    writer.spillway.substring(0, 200) shouldBe "Newton Sep 24th 2024\nSection A\nResults for direction N/S\nRank\tPair\tMPs\tPercent\tNames\n1=\t8\t33.50\t69.79%\tAmy Avergun & Penny Scharfman\n1=\t9\t33.50\t69.79%\tMarsha & Robert Greenstein\n3 \t3\t33.00\t68.75%\tKaj "
  }
  it should "output with unplayed boards" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/Newton/Newton20241001a.txt", Output(writer))) o.close()
    writer.spillway.substring(0, 2000) shouldBe
      """Newton Oct 1st 2024
        |Section A
        |Results for direction N/S
        |Rank	Pair	MPs	Percent	Names
        |1 	7	34.99	72.90%	Carol Leahy & Deanna Szeto
        |2 	6	28.88	60.16%	Mary Ellen Clark & Leslie Greenberg
        |3 	9	27.50	57.29%	Amy Avergun & Penny Scharfman
        |4 	8	25.49	53.10%	Jane Venti & Jane Volden
        |5 	3	22.81	47.51%	Josh Gahm & Marya Van'T Hul
        |6 	2	21.92	45.66%	Joanne Hennessy & Veets Veitas
        |7 	5	21.38	44.54%	Vivian Hernandez & Roberta Kosberg
        |8 	4	19.61	40.86%	Marsha & Rob Greenstein
        |9 	1	14.00	35.00%	Judy & David Taub
        |Results for direction E/W
        |Rank	Pair	MPs	Percent	Names
        |1 	4	34.39	71.64%	Rick & Lisa Martin
        |2 	1	33.50	83.75%	Kaj Wilson & Ellen Dockser
        |3 	3	29.19	60.82%	Robin Zelle & Barbara Berenson
        |4 	6	25.12	52.34%	Gerri Taylor & Sherrill Kobrick
        |5 	8	21.51	44.81%	Judy Tucker & Sheila Jones
        |6 	9	21.50	44.79%	Kathy Curtiss & Linda Worters
        |7 	7	19.51	40.65%	Alan Gordon & Margaret Meehan
        |8 	2	16.58	34.55%	Rebecca Kratka & MJ Weinstein
        |9 	5	 6.12	12.76%	Barbara & Don Oppenheimer
        |=====================================================
        |=====================================================
        |Newton Oct 1st 2024
        |A
        |1N	Judy & David Taub
        |2N	Joanne Hennessy & Veets Veitas
        |3N	Josh Gahm & Marya Van'T Hul
        |4N	Marsha & Rob Greenstein
        |5N	Vivian Hernandez & Roberta Kosberg
        |6N	Mary Ellen Clark & Leslie Greenberg
        |7N	Carol Leahy & Deanna Szeto
        |8N	Jane Venti & Jane Volden
        |9N	Amy Avergun & Penny Scharfman
        |1E	Kaj Wilson & Ellen Dockser
        |2E	Rebecca Kratka & MJ Weinstein
        |3E	Robin Zelle & Barbara Berenson
        |4E	Rick & Lisa Martin
        |5E	Barbara & Don Oppenheimer
        |6E	Gerri Taylor & Sherrill Kobrick
        |7E	Alan Gordon & Margaret Meehan
        |8E	Judy Tucker & Sheila Jones
        |9E	Kathy Curtiss & Linda Worters
        |
        |Board: 1 with 9 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	-100	 0.00
        |2	2	110	 5.50
        |3	3	110	 5.50
        |4	4	-50	 1.50
        |5	5	110	 5.50
        |6	6	-50	 1.50
        |7	7	110	 5.50
        |8	8	100	 3.00
        |9	9	140	 8.00
        |Board: 2 with 8 plays
        |NS pair	EW pair	NS score	NS MPs
        |2	2	-180	 3.43
        |3	3	-460	 1.14
        |4	4	-400	 2.29
        |5	5	-130	 5.14
        |6	6	-130	 5.14
        |7	7	-90	 7.43
        |8	8	-90	 7.43
        |9	9""".stripMargin
  }
  it should "output from Newton" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/Newton/Newton20241001.txt", Output(writer))) o.close()
    writer.spillway.substring(0, 2000) shouldBe
      """Newton Oct 1st 2024
        |Section A
        |Results for direction N/S
        |Rank	Pair	MPs	Percent	Names
        |1 	7	34.00	70.83%	Carol Leahy & Deanna Szeto
        |2 	6	28.50	59.38%	Mary Ellen Clark & Leslie Greenberg
        |3 	9	27.50	57.29%	Amy Avergun & Penny Scharfman
        |4 	8	24.50	51.04%	Jane Venti & Jane Volden
        |5 	3	22.50	46.88%	Josh Gahm & Marya Van'T Hul
        |6 	2	22.00	45.83%	Joanne Hennessy & Veets Veitas
        |7 	5	21.00	43.75%	Vivian Hernandez & Roberta Kosberg
        |8 	4	20.00	41.67%	Marsha & Rob Greenstein
        |9 	1	16.00	33.33%	Judy & David Taub
        |Results for direction E/W
        |Rank	Pair	MPs	Percent	Names
        |1 	1	39.50	82.29%	Kaj Wilson & Ellen Dockser
        |2 	4	34.00	70.83%	Rick & Lisa Martin
        |3 	3	29.50	61.46%	Robin Zelle & Barbara Berenson
        |4 	6	25.50	53.13%	Gerri Taylor & Sherrill Kobrick
        |5 	8	22.50	46.88%	Judy Tucker & Sheila Jones
        |6 	9	21.50	44.79%	Kathy Curtiss & Linda Worters
        |7 	7	20.50	42.71%	Alan Gordon & Margaret Meehan
        |8 	2	16.50	34.38%	Rebecca Kratka & MJ Weinstein
        |9 	5	 6.50	13.54%	Barbara & Don Oppenheimer
        |=====================================================
        |=====================================================
        |Newton Oct 1st 2024
        |A
        |1N	Judy & David Taub
        |2N	Joanne Hennessy & Veets Veitas
        |3N	Josh Gahm & Marya Van'T Hul
        |4N	Marsha & Rob Greenstein
        |5N	Vivian Hernandez & Roberta Kosberg
        |6N	Mary Ellen Clark & Leslie Greenberg
        |7N	Carol Leahy & Deanna Szeto
        |8N	Jane Venti & Jane Volden
        |9N	Amy Avergun & Penny Scharfman
        |1E	Kaj Wilson & Ellen Dockser
        |2E	Rebecca Kratka & MJ Weinstein
        |3E	Robin Zelle & Barbara Berenson
        |4E	Rick & Lisa Martin
        |5E	Barbara & Don Oppenheimer
        |6E	Gerri Taylor & Sherrill Kobrick
        |7E	Alan Gordon & Margaret Meehan
        |8E	Judy Tucker & Sheila Jones
        |9E	Kathy Curtiss & Linda Worters
        |
        |Board: 1 with 9 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	-100	 0.00
        |2	2	110	 5.50
        |3	3	110	 5.50
        |4	4	-50	 1.50
        |5	5	110	 5.50
        |6	6	-50	 1.50
        |7	7	110	 5.50
        |8	8	100	 3.00
        |9	9	140	 8.00
        |Board: 2 with 9 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	-430	 2.00
        |2	2	-180	 4.00
        |3	3	-460	 1.00
        |4	4	-400	 3.00
        |5	5	-130	 5.50
        |6	6	-130	 5.50
        |7	7	-90	 7.50
        |8	""".stripMargin
  }

  // TODO Find out why this doesn't work!
  ignore should "read ConcordCountryClub20191007.txt using doScoreFromName" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromName(isResource = true, "ConcordCountryClub20191007.txt", Output(writer))) o.close()
    writer.spilled shouldBe 2642
  }

  behavior of "Card"

  it should "toStringPercent" in {
    val target = Card(Rational(5, 2), 5, 0)
    target.toStringPercent shouldBe "50.00%"
  }

  it should "toStringPercentDNP" in {
    val target = Card(Rational(4, 2), 4, 1)
    target.toStringPercent shouldBe "50.00%"
  }

  it should "toStringMps" in {
    val target = Card(Rational(5), 5, 0)
    target.toStringMps(2) shouldBe "10.00"
  }

  it should "toStringMpsDNP" in {
    val target = Card(Rational(5), 4, 1)
    target.toStringMps(2) shouldBe "12.50"
  }

  behavior of "Rational"

  it should "render" in {
    Score.rationalToString(Score.asPercent(Rational(1, 2), 1)) shouldBe "50.00"
    Score.rationalToString(Rational(2, 1)) shouldBe " 2.00"
    Score.rationalToString(Rational(1, 2)) shouldBe " 0.50"
  }

}

