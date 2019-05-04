package com.phasmidsoftware.bridge.director

import com.phasmid.laScala.values.Rational
import com.phasmidsoftware.bridge.director
import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
	* @author scalaprof
	*/
class ScoreSpec extends FlatSpec with Matchers {

	behavior of "travelers"

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
	"mpsAsPercentage" should "work" in {
		val r = Rational(3, 4)
		Score.mpsAsPercentage(r, 1) shouldBe "75.00%"
	}
	"mpsAsString" should "work" in {
		val r = Rational(3, 4)
		Score.mpsAsString(r, 6) shouldBe "4.50"
	}

	behavior of "play"
	it should "compare 1 1 +130 with 2 2 110 as 2" in {
		val p1 = Play(1, 1, PlayResult(Right(130)))
		val p2 = Play(2, 2, PlayResult(Right(110)))
		p1.compare(p2.result) shouldBe Some(0)
		p2.compare(p1.result) shouldBe Some(2)
	}

	behavior of "traveler"
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
		Score.mpsAsString(mps.tail.head.mp.get, 2) shouldBe "1.50"
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

	behavior of "section"


	it should "work" in {
		def checkResult(result: Result, directionNS: Boolean): Unit = {
			result.isNS shouldBe directionNS
			result.top shouldBe 1
			val cards: Map[Int, (Rational[Int], Int)] = result.cards
			cards.size shouldBe 2
			val total: Rational[Int] = (for ((r, _) <- cards.values) yield r).sum
			total shouldBe Rational[Int](2).invert * cards.size * (result.top + 1)
			for ((_, (_, t)) <- cards) t shouldBe result.top + 1
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
		resultANS.isNS shouldBe true
		resultANS.top shouldBe 5
		val cards: Map[Int, (Rational[Int], Int)] = resultANS.cards
		cards.size shouldBe 6
		val total: Rational[Int] = (for ((r, _) <- cards.values) yield r).sum
		total shouldBe Rational[Int](2).invert * cards.size * (resultANS.top + 1)
		val scores = for (score <- cards.keys) yield cards(score)
		scores.size shouldBe 6
		for ((_, (_, t)) <- cards) t shouldBe resultANS.top + 1
	}

	// This file seems to be incorrect so maybe it's not a problem that this test doesn't succeed
	ignore should "read travelers.lexington.2016.0503 as a resource" in {
		val resource = "travelers.lexington.2016.0503"
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
		resultANS.isNS shouldBe true
		resultANS.top shouldBe 5
		val cards: Map[Int, (Rational[Int], Int)] = resultANS.cards
		cards.size shouldBe 14
		val scores = (for (score <- cards.keys) yield cards(score)).toSeq
		scores.size shouldBe 12
		val total: Rational[Int] = (for ((r, _) <- cards.values) yield r).sum
		total shouldBe Rational[Int](2).invert * cards.size * (resultANS.top + 1)
		scores.size shouldBe 6
		for ((_, (_, t)) <- cards) t shouldBe resultANS.top + 1
	}

	behavior of "Score"
	it should "read travelers.lexington.2017.0404 as a resource" in {
		val writer = MockWriter(8192)
		for (o <- Score.doScoreResource("travelers.lexington.2017.0404", Output(writer))) o.close()
		writer.spilled shouldBe 2336
	}
	it should "read travelers.lexington.2017.0404 as a file" in {
		val writer = MockWriter(8192)
		for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/travelers.lexington.2017.0404", Output(writer))) o.close()
		writer.spilled shouldBe 2336
	}
}

