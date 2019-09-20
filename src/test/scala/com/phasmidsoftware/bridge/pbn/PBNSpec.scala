/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.pbn

import com.phasmidsoftware.bridge.cards.{Deal, Whist}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.{Success, Try}

class PBNSpec extends FlatSpec with Matchers {

  private val py: Try[PBN] = PBNParser.parsePBN(Source.fromResource("com/phasmidsoftware/bridge/director/LEXINGTON 2016.2.9.PBN"))

  behavior of "PBN"

  it should "iterator" in {
    val dsy = for (p <- py) yield for (g: Game <- p) yield g("Board")
    dsy should matchPattern { case Success(_) => }
    dsy.get.headOption should matchPattern { case Some(DetailedValue(_, _)) => }
  }

  it should "games" in {
    py.foreach(_.games.headOption.get("Board") shouldBe DetailedValue(StringValue("1"), Nil))
  }

  it should "headOption" in {
    py.foreach(_.headOption.get("Board") shouldBe DetailedValue(StringValue("1"), Nil))
  }

  it should "get first board number" in {
    py.get.headOption.get("Board").toInt shouldBe 1
  }

  it should "sort" in {
    py.get.sorted.head("Board").toInt shouldBe 1
  }

  behavior of "StringValue"
  it should "toInt" in {
    val target = StringValue("1")
    target.toInt shouldBe 1
  }

  behavior of "DealValue"
  it should "getDeal" in {
    val deal = py.get.head("Deal").value.asInstanceOf[DealValue].deal
    deal should matchPattern { case Deal(_, _) => }
  }
  ignore should "analyze deal" in {
    val deal = py.get.head("Deal").value.asInstanceOf[DealValue].deal
    //noinspection ScalaStyle
    Whist(deal, 1).analyzeDoubleDummy(8, directionNS = true) shouldBe Some(true)
  }

  behavior of "DetailedValue"
  it should "trim last newline 1" in {
    val xs = Seq("Hello\n", "World\n")
    val target = DetailedValue.trim(StringValue(""), xs)
    target.detail shouldBe Seq("Hello\n", "World")
  }
  it should "trim last newline 2" in {
    val xs = Seq("Hello\n", "\n")
    val target = DetailedValue.trim(StringValue(""), xs)
    target.detail shouldBe Seq("Hello\n")
  }
  it should "toInt" in {
    val target = DetailedValue.trim(StringValue("1"), Nil)
    target.toInt shouldBe 1
  }
}
