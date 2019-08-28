/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Try

//noinspection ScalaStyle
class WhistPBNSpec extends FlatSpec with Matchers with TimeLimitedTests{

  val timeLimit = Span(10, Seconds)

  private val py: Try[PBN] = PBNParser.parsePBN(Source.fromResource("com/phasmidsoftware/bridge/director/LEXINGTON 2016.2.9.PBN"))
  private val pbn: PBN = py.get

  behavior of "double dummy analysis and PBN parser"
  it should "analyze deal 0" in {
    val game = pbn.head
    analyzeMakableContracts(game)
  }

  it should "analyze deal 1" in {
    val game = pbn(1)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 2" in {
    val game = pbn(2)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 3" in {
    val game = pbn(3)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 4" in {
    val game = pbn(4)
    analyzeMakableContracts(game)
  }

  // 8 seconds
  it should "analyze deal 5" in {
    val game = pbn(5)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 6" in {
    val game = pbn(6)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 7" in {
    val game = pbn.last
    analyzeMakableContracts(game)
  }

  private def analyzeMakableContracts(game: Game): Unit = {
    val deal = game("Deal").value.asInstanceOf[DealValue].deal
    val detail = game("OptimumResultTable").detail
    val ntContracts = detail.filter(_.contains("NT")).filter(_.startsWith("S"))
    val declarerTricksR = """([NESW])\s*NT\s*(\d+)""".r
    ntContracts foreach {
      case declarerTricksR(l, n) =>
        val declarer = "NESW".indexOf(l)
        val leader = Hand.next(declarer)
        val tricks = n.toInt
        println(s"analyzeDoubleDummy: tricks=$tricks, declarer=$l, leader=$leader")
        Whist(deal, leader).analyzeDoubleDummy(tricks, directionNS = declarer % 2 == 0) shouldBe Some(true)
    }
  }
}
