/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmid.laScala.values.Rational
import com.phasmidsoftware.bridge.director
import com.phasmidsoftware.output.MockWriter
import com.phasmidsoftware.util.Output
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * @author scalaprof
  */
class ScoreSpec extends FlatSpec with Matchers {

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
    target.matchpoints(None) shouldBe Some(Rational[Int](1) / 2)
    target.toString shouldBe "A"
  }
  it should "apply(A+)" in {
    val target = PlayResult("A+")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational[Int](3) / 5)
    target.toString shouldBe "A+"
  }
  it should "apply(A-)" in {
    val target = PlayResult("A-")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational[Int](2) / 5)
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
    maybeTraveler should matchPattern { case Some(Traveler(_, _)) => }
    val t = maybeTraveler.get
    t.board shouldBe board
    t.ps.size shouldBe 1
    t.ps.head shouldBe play
  }
  it should "addTo non-empty traveler map" in {
    val board = 1
    val play1 = Play(1, 2, PlayResult("110"))
    val play2 = Play(3, 4, PlayResult("100"))
    val travelerMap = Map[Int, Traveler](1 -> Traveler(1, Seq(play2)))
    val target = BoardPlay(board, play1)
    val updatedMap = target.addTo(travelerMap)
    updatedMap.size shouldBe 1
    val maybeTraveler = updatedMap.get(board)
    maybeTraveler should matchPattern { case Some(Traveler(_, _)) => }
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
    val target = Pickup(ns, ew, Seq(BoardResult(bd1, result1), BoardResult(bd2, result2)))
    target.toString shouldBe s"Pickup: $ns vs $ew: $bd1: $score1, $bd2: $score2"
    target.boards.size shouldBe 2
    target.ns shouldBe ns
    target.ew shouldBe ew
    val boardPlays: Seq[BoardPlay] = target.asBoardPlays
    boardPlays shouldBe Seq(boardPlay1, boardPlay2)
  }

  behavior of "Traveler"
  it should "matchpoint properly (1)" in {
    val p1 = Play(2, 1, PlayResult(Right(130)))
    val p2 = Play(1, 2, PlayResult(Right(150)))
    val t = Traveler(1, Seq(p1, p2))
    t.matchpoint(p1) shouldBe Some(Rational.zero[Int])
    t.matchpoint(p2) shouldBe Some(Rational.one[Int])
  }
  it should "matchpoint properly (2)" in {
    val p1 = Play(2, 1, PlayResult(Right(130)))
    val p2 = Play(1, 2, PlayResult(Right(150)))
    val t = Traveler(1, Seq(p1, p2))
    val mps = t.matchpointIt
    mps.head.mp shouldBe Some(Rational.zero[Int])
    mps.tail.head.mp shouldBe Some(Rational.one[Int])
  }
  it should "matchpoint properly (3)" in {
    val p1 = Play(2, 1, PlayResult(Right(130)))
    val p2 = Play(1, 2, PlayResult(Right(150)))
    val p3 = Play(3, 3, PlayResult(Right(150)))
    val t = Traveler(1, Seq(p1, p2, p3))
    val mps = t.matchpointIt
    mps.head.mp shouldBe Some(Rational.zero[Int])
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
    mps shouldBe Some(Rational.zero[Int])
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
    val target = Traveler(bd, Seq(play1, play2))
    target.board shouldBe bd
    target.ps.size shouldBe 2
    target.ps shouldBe Seq(play1, play2)
    target.isPlayed shouldBe true
    target.top shouldBe 1
    val writer = MockWriter()
    target.output(Output(writer)).close()
    writer.spillway shouldBe s"Board: $bd with 2 plays\nNS: $ns1, EW: $ew1, score: $score1, MP: 1.00\nNS: $ns2, EW: $ew2, score: $score2, MP: 0.00\n"
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
    val target = Traveler(bd, Seq(play1, play2))
    target.board shouldBe bd
    target.ps.size shouldBe 2
    target.ps shouldBe Seq(play1, play2)
    target.isPlayed shouldBe true
    target.top shouldBe 1
    target.toString shouldBe s"Traveler($bd,List($ns1 vs $ew1: $score1, $ns2 vs $ew2: $score2))"
  }
  it should "matchpoint" in {
    val play1 = Play(13, 17, PlayResult("110"))
    val play2 = Play(14, 16, PlayResult("100"))
    val target = Traveler(5, Seq(play1, play2))
    target.matchpoint(play1) shouldBe Some(Rational[Int](1))
    target.matchpoint(play2) shouldBe Some(Rational[Int](0))
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
    val target = Traveler(bd, Seq(Play(ns1, ew1, result1), Play(ns2, ew2, result2)))
    val top = 1
    val matchpoints1 = Matchpoints(ns1, ew1, result1, Some(Rational[Int](1)), top)
    val matchpoints2 = Matchpoints(ns2, ew2, result2, Some(Rational[Int](0)), top)
    target.matchpointIt shouldBe Seq(matchpoints1, matchpoints2)
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
    val target5 = Traveler(bd1, Nil)
    target5 :+ play1 shouldBe Traveler(bd1, Seq(play1))
    val target6 = Traveler(bd2, Nil)
    target6 :+ play2 shouldBe Traveler(bd2, Seq(play2))
  }

  behavior of "pairs"

  behavior of "player"

  behavior of "playerPlayer"

  behavior of "pair"

  behavior of "preamble"

  behavior of "result"
  it should "score DNP as None" in {
    val p1 = Play(1, 1, PlayResult(Left("DNP")))
    val t = Traveler(1, Seq())
    p1.matchpoints(t) shouldBe None
  }
  it should "score A as Some(1/2)" in {
    val p1 = Play(1, 1, PlayResult(Left("A")))
    val t = Traveler(1, Seq())
    p1.matchpoints(t) shouldBe Some(Rational(1, 2))
  }
  it should "score A- as Some(2,5)" in {
    val p1 = Play(1, 1, PlayResult(Left("A-")))
    val t = Traveler(1, Seq())
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
    val pairs = Seq(
      director.Pair(1, "N", (Player("tweedledum"), Player("tweedledee"))),
      director.Pair(2, "N", (Player("James Clark Maxwell"), Player("Albert Einstein"))),
      director.Pair(1, "E", (Player("Tristan"), Player("Isolde"))),
      director.Pair(2, "E", (Player("Romeo"), Player("Juliet")))
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
    val traveler1 = Traveler(bd1, Seq(Play(ns1, ew1, result111), Play(ns2, ew2, result122)))
    val traveler2 = Traveler(bd2, Seq(Play(ns2, ew2, result222), Play(ns2, ew1, result221)))
    val travelers: Seq[Traveler] = Seq()
    val pickup111 = Pickup(ns1, ew1, Seq(BoardResult(bd1, result111)))
    val pickup122 = Pickup(ns2, ew2, Seq(BoardResult(bd1, result122)))
    val pickup221 = Pickup(ns2, ew1, Seq(BoardResult(bd2, result221)))
    val pickup222 = Pickup(ns2, ew2, Seq(BoardResult(bd2, result222)))
    val pickups = Seq(pickup111, pickup122, pickup222, pickup221)
    val target = Section(preamble, travelers, pickups)
    target.travelers.toList shouldBe Seq(traveler1, traveler2)
  }

  it should "work" in {
    def checkResult(result: Result, directionNS: Boolean): Unit = {
      result.isNS shouldBe Some(directionNS)
      result.top shouldBe 1
      val cards: Map[Int, Card] = result.cards
      cards.size shouldBe 2
      val total: Rational[Int] = (for (Card(r, _, _) <- cards.values) yield r).sum
      total shouldBe Rational[Int](2).invert * cards.size * (result.top + 1)
      for ((_, Card(_, t, _)) <- cards) t shouldBe result.top + 1
    }

    val pairs = Seq(
      director.Pair(1, "N", (Player("tweedledum"), Player("tweedledee"))),
      director.Pair(2, "N", (Player("James Clark Maxwell"), Player("Albert Einstein"))),
      director.Pair(1, "E", (Player("Tristan"), Player("Isolde"))),
      director.Pair(2, "E", (Player("Romeo"), Player("Juliet")))
    )
    val travelers: Seq[Traveler] = Seq(
      Traveler(1, Seq(Play(1, 1, PlayResult(Right(110))), Play(2, 2, PlayResult(Right(100))))),
      Traveler(2, Seq(Play(1, 2, PlayResult(Right(-400))), Play(2, 1, PlayResult(Right(-430)))))
    )
    val preamble = Preamble("A", None, pairs)
    val section = Section(preamble, travelers)
    section.calculateTop shouldBe 1
    val results: Seq[Result] = section.createResults
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
    val total: Rational[Int] = (for (Card(r, _, _) <- cards.values) yield r).sum
    total shouldBe Rational[Int](2).invert * cards.size * (resultANS.top + 1)
    val scores = for (score <- cards.keys) yield cards(score)
    scores.size shouldBe 6
    for ((_, Card(_, t, _)) <- cards) t shouldBe resultANS.top + 1
  }

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
  //		val cards: Map[Int, (Rational[Int], Int)] = resultANS.cards
  //		cards.size shouldBe 14
  //		val scores = (for (score <- cards.keys) yield cards(score)).toSeq
  //		scores.size shouldBe 12
  //		val total: Rational[Int] = (for ((r, _) <- cards.values) yield r).sum
  //		total shouldBe Rational[Int](2).invert * cards.size * (resultANS.top + 1)
  //		scores.size shouldBe 6
  //		for ((_, (_, t)) <- cards) t shouldBe resultANS.top + 1
  //	}

  behavior of "Score"
  it should "read travelers.lexington.2017.0404 as a resource" in {
    val writer = MockWriter(8192)
    val output = Output(writer)
    for (o <- Score.doScoreResource("travelers.lexington.2017.0404", output)) o.close()
    writer.spilled shouldBe 2325
  }
  it should "read travelers.lexington.2017.0404P as a resource (includes pickup slips)" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreResource("travelers.lexington.2017.0404P", Output(writer))) o.close()
    writer.spilled shouldBe 2325
  }
  it should "read travelers.lexington.2017.0404 as a file" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/travelers.lexington.2017.0404", Output(writer))) o.close()
    writer.spilled shouldBe 2325
  }

  //noinspection SpellCheckingInspection
  it should "read keremshalom.2019.0509.txt as a file" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/keremshalom.2019.0509.txt", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    writer.spillway shouldBe "Kerem Shalom Beginner Duplicate: May 9th 2019\nSection A\nResults for direction N/S\n1 :  6.00 : 75.00% : Dan & Tenley\n3 :  5.67 : 56.67% : Chris & Kathy\n2 :  5.17 : 43.06% : Irene & Robin\n4 :  3.17 : 31.67% : Tom & Jane\nResults for direction E/W\n4 :  6.83 : 68.33% : Ghilaine & Bill\n2 :  3.83 : 47.92% : JoAnn & Margaret\n1 :  4.33 : 43.33% : Marian & Patty\n3 :  5.00 : 41.67% : Wendy & Ruth\n=====================================================\n=====================================================\nKerem Shalom Beginner Duplicate: May 9th 2019\nA\n1N: Dan & Tenley\n1E: Marian & Patty\n2N: Irene & Robin\n2E: JoAnn & Margaret\n3N: Chris & Kathy\n3E: Wendy & Ruth\n4N: Tom & Jane\n4E: Ghilaine & Bill\n\nBoard: 1 with 4 plays\nNS: 1, EW: 1, score: 450, MP: 1.50\nNS: 2, EW: 3, score: 450, MP: 1.50\nNS: 4, EW: 4, score: 450, MP: 1.50\nNS: 3, EW: 2, score: 450, MP: 1.50\nBoard: 2 with 3 plays\nNS: 1, EW: 1, score: -980, MP: 1.00\nNS: 2, EW: 3, score: -1010, MP: 0.00\nNS: 3, EW: 2, score: -510, MP: 2.00\nBoard: 3 with 4 plays\nNS: 2, EW: 2, score: -650, MP: 1.00\nNS: 4, EW: 4, score: -650, MP: 1.00\nNS: 1, EW: 3, score: 50, MP: 3.00\nNS: 3, EW: 1, score: -650, MP: 1.00\nBoard: 4 with 3 plays\nNS: 2, EW: 2, score: -100, MP: 0.50\nNS: 4, EW: 4, score: -100, MP: 0.50\nNS: 1, EW: 3, score: 1430, MP: 2.00\nBoard: 5 with 3 plays\nNS: 3, EW: 3, score: 50, MP: 1.00\nNS: 2, EW: 1, score: 50, MP: 1.00\nNS: 4, EW: 4, score: 50, MP: 1.00\nBoard: 6 with 3 plays\nNS: 3, EW: 3, score: 450, MP: 1.00\nNS: 2, EW: 1, score: 480, MP: 2.00\nNS: 4, EW: 4, score: -50, MP: 0.00\n"
    writer.spilled shouldBe 1529
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

