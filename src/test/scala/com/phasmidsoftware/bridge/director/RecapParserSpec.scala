/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmid.laScala.values.Rational
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * @author scalaprof
  */
class RecapParserSpec extends FlatSpec with Matchers {
  behavior of "endOfLine"
  it should "parse" in {
    val parser = new RecapParser
    parser.parseAll(parser.endOfLine, "\n") should matchPattern { case parser.Success(_, _) => }
    parser.parseAll(parser.endOfLine, sys.props("line.separator")) should matchPattern { case parser.Success(_, _) => }
  }

  behavior of "title"
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.title, "Test Section 2016/04/12")
    r should matchPattern { case parser.Success(_, _) => }
  }

  behavior of "travelers"
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.travelers, "T 1\n1 1 130\n2 2 150\n\nT 2\n1 1 130\n2 2 150\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.size shouldBe 2
  }
  it should "parse with carriage returns" in {
    val parser = new RecapParser
    val newline = sys.props("line.separator")
    val r = parser.parseAll(parser.travelers, s"T 1$newline 1 1 130$newline 2 2 150$newline$newline T 2$newline 1 1 130$newline 2 2 150$newline$newline")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.size shouldBe 2
  }

  it should "parse without final newline" in {
    val parser = new RecapParser
    val result = parser.parseAll(parser.travelers, "T 1\n1 1 130\n2 2 150\n\nT 2\n1 1 130\n2 2 150\n")

    result match {
      case parser.Success(_, _) =>
      case _ => fail(s"needs extra newline")
    }
  }

  behavior of "pairs"
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.pairs, "1 N Erithacus Rubecula & Esox Lucius\n")
    r should matchPattern { case parser.Success(_, _) => }
    val pairs = r.get
    pairs.size shouldBe 1
  }

  behavior of "player"
  it should "parse without leading space" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.player, "Esox Lucius  ")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.name shouldBe "Esox Lucius"
  }
  it should "parse with leading space" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.player, " Esox Lucius  ")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.name shouldBe "Esox Lucius"
  }

  behavior of "playerPlayer"
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.playerPlayer, "Erithacus Rubecula & Esox Lucius ")
    r should matchPattern { case parser.Success(_, _) => }
    val pair = r.get
    pair._1.name shouldBe "Erithacus Rubecula"
    pair._2.name shouldBe "Esox Lucius"
  }

  behavior of "pair"
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.pair, "1 N Erithacus Rubecula & Esox Lucius")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.number shouldBe 1
    r.get.direction shouldBe "N"
    r.get.players should matchPattern { case (_, _) => }
    r.get.players._1 shouldBe Player("Erithacus Rubecula")
  }

  behavior of "preamble"
  it should "parse without modifier" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.preamble, "A\n1 N Erithacus Rubecula & Esox Lucius\n")
    r should matchPattern { case parser.Success(_, _) => }
    val preamble = r.get
    preamble.maybeModifier shouldBe None
    preamble.identifier shouldBe "A"
    preamble.pairs.size shouldBe 1
  }

  it should "parse with modifier" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.preamble, "A SW\n1 N Erithacus Rubecula & Esox Lucius\n")
    r should matchPattern { case parser.Success(_, _) => }
    val preamble = r.get
    preamble.identifier shouldBe "A"
    preamble.maybeModifier shouldBe Some("SW")
    preamble.pairs.size shouldBe 1
  }

  behavior of "result"
  it should "parse 130" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.result, "130")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case PlayResult(Right(130)) => }
  }
  it should "parse DNP" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.result, "DNP")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case PlayResult(Left("DNP")) => }
  }
  it should "parse A-" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.result, "A-")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case PlayResult(Left("A-")) => }
  }

  behavior of "play"
  it should "parse 1 1 130" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.play, "1 1 130")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.ns shouldBe 1
    r.get.ew shouldBe 1
    r.get.result should matchPattern { case PlayResult(Right(130)) => }
  }
  it should "parse  1 1 130" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.play, " 1 1 130")
    r should matchPattern { case parser.Success(_, _) => }
  }

  behavior of "pickup"
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.pickup, "P 13 17\n1  130\n2  150\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    val pickup: Pickup = r.get
    pickup.ns shouldBe 13
    pickup.ew shouldBe 17
    pickup.boards.size shouldBe 2
  }
  behavior of "traveler"
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.traveler, "T 1\n1 1 130\n2 2 150\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.board shouldBe 1
    r.get.ps.size shouldBe 2
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
  it should "parse" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.section, "A\n1 N Erithacus Rubecula & Esox Lucius\nT 1\n1 1 130\n2 2 150\n\nT 2\n1 1 130\n2 2 150\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.preamble.identifier shouldBe "A"
    r.get.preamble.pairs.size shouldBe 1
    r.get.travelers.size shouldBe 2
  }

  it should "parse with tabs" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.section, "A\n1 N Erithacus Rubecula & Esox Lucius\nT\t1\n1 1\t130\n2 2\t150\n\nT 2\n1 1\t130\n2 2\t150\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.preamble.identifier shouldBe "A"
    r.get.preamble.pairs.size shouldBe 1
    r.get.travelers.size shouldBe 2
  }

  behavior of "event"
  it should "parse mock event" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.event, "Test Section 2016/04/12\nA\n1 N Erithacus Rubecula & Esox Lucius\nT 1\n1 1 130\n2 2 150\n\nT 2\n1 1 130\n2 2 150\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    val event: Event = r.get
    event.title shouldBe "Test Section 2016/04/12"
    event.sections.size shouldBe 1
    val section: Section = event.sections.head
    section.preamble shouldBe Preamble("A", None, Seq(Pair(1, "N", Player("Erithacus Rubecula") -> Player("Esox Lucius"))))
  }

  it should "parse single-winner event" in {
    val parser = new RecapParser
    val r = parser.parseAll(parser.event, "Dummy's Long Suit: May 3rd 2016\nA SW\n1 N Sue & Jim\n2 N Sally & Sara\n3 N Ralph & Sandra\n4 N Carol & Mary\n5 N Ashley & Peter\n6 N Angela & Judy\n7 N Jennifer & Lou\n8 N Jane & Nina\n9 N Shelly & Ray\n10 N Glenn & Alicia\n11 N Linda & Jean\n12 N Terry & Jane\n13 N Janice & PJ\n14 N Sue & Pete\nT 1\n1 8 450\n3 12 140\n4 7 450\n5 9 -50\n6 11 -50\n14 13 420\n\nT 2\n1 7 -510\n2 9 -980\n4 14 -980\n5 8 -480\n6 10 -450\n13 12 -1010\n\nT 3\n1 14 450\n2 8 -100\n3 10 480\n4 13 980\n5 7 450\n6 9 200\n12 11 980\n\nT 4\n1 13 -200\n2 7 -170\n3 9 -170\n5 14 -620\n6 8 -620\n11 10 100\n\nT 5\n1 12 420\n2 14 -100\n3 8 -100\n4 11 180\n6 7 -100\n10 9 170\n\nT 6\n1 11 480\n2 13 980\n3 7 480\n4 10 480\n5 12 980\n9 8 480\n\nT 7\n1 10 100\n2 12 -620\n3 14 -620\n4 9 -120\n6 13 100\n5 11 -650\n\nT 8\n2 11 50\n3 13 -140\n4 8 -450\n5 10 -420\n6 12 -420\n7 14 50\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    val event: Event = r.get
    event.title shouldBe "Dummy's Long Suit: May 3rd 2016"
    event.sections.size shouldBe 1
    val section: Section = event.sections.head
    section.preamble should matchPattern { case Preamble("A", Some("SW"), _) => }
    section.travelers.size shouldBe 8
  }

  it should "parse double-winner event with tabs" in {
    val parser = new RecapParser
    val r: parser.ParseResult[Event] = parser.parseAll(parser.event, "Lexington: Apr 4th 2017\nA DW\n1\tN Michele & Jean\n2   N Kathryn & Terry\n3\tN Kathy & Thornton\n3   E Dave & Bill\n2\tE Homer & Chuck\n1   E Glen & Janis\n23\tN Ray & Shelly\n21  N Teunis & John\n22\tN Linda & Chris\n22  E Barbara & Cynthia\n21\tE Kim & Clay\n23 E Jennifer & Karolyn\nT 1\n1\t1 450\n2\t3 -50\n3\t2 -50\n21\t21 -50\n22\t23 980\n23\t22 510\n\nT 2\n1\t1 50\n2\t3  50\n3\t2  50\n21\t21 50\n22\t23 -420\n23\t22 50\n\nT 3\n1\t3 400\n2\t2 150\n3\t1 120\n21\t23 -50\n22\t22 -50\n23\t21 -100\n\nT 4\n1\t3 200\n2\t2 200\n3\t1  -110\n21\t23 -110\n22\t22 300\n23\t21 100\n\nT 5\n1\t2 -300\n2\t1 -200\n3\t3 -500\n21\t22 -50\n22\t21 -300\n23\t23 110\n\nT 6\n1\t2 -680\n2\t1 -1430\n3\t3 -650\n21\t22 -1460\n22\t21 -1430\n23\t23 -650\n\n")
    r should matchPattern { case parser.Success(_, _) => }
    val event: Event = r.get
    event.title shouldBe "Lexington: Apr 4th 2017"
    event.sections.size shouldBe 1
    val section: Section = event.sections.head
    section.preamble should matchPattern { case Preamble("A", Some("DW"), _) => }
    section.travelers.size shouldBe 6
  }

  it should "read travelers.lexington.2016.0503 as a resource" in {
    val resource = "travelers.lexington.2016.0503"
    val ey: Try[Event] = Option(getClass.getResourceAsStream(resource)) match {
      case Some(s) => RecapParser.readEvent(Source.fromInputStream(s))
      case None => Failure(new Exception(s"doScoreResource: cannot open resource: $resource"))
    }
    ey should matchPattern { case Success(Event(_, _)) => }
    val event = ey.get
    event.title shouldBe "Dummy's Long Suit: May 3rd 2016"
    val sections = event.sections
    sections.size shouldBe 1
    val a = sections.head
    val preamble = a.preamble
    preamble should matchPattern { case Preamble("A", Some("SW"), _) => }
    val pairs = preamble.pairs
    pairs.size shouldBe 14
    val pair = pairs.head
    pair.number shouldBe 1
    pair.players shouldBe(Player("Sue"), Player("Jim"))
    pair.direction shouldBe "N"
  }
}

