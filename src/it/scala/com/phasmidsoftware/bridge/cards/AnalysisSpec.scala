/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

//noinspection ScalaStyle
class AnalysisSpec extends FlatSpec with Matchers with TimeLimitedTests {

  val timeLimit = Span(23, Seconds)

  State.count = 0
  private val so = Option(getClass.getResourceAsStream("westwood_20190625_1.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

  //  Flog.enabled = false

  behavior of "double dummy analysis"
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

  // 22 seconds
  it should "analyze deal 3" in {
    val game = pbn(3)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 4" in {
    val game = pbn(4)
    analyzeMakableContracts(game)
  }

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

  it should "analyze deal 16" in {
    val game = pbn(15)
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
