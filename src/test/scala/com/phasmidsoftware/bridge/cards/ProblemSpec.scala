/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import com.phasmidsoftware.util.Flog
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

//noinspection ScalaStyle
class ProblemSpec extends FlatSpec with Matchers {

  State.count = 0
  private val so = Option(getClass.getResourceAsStream("westwood_20190625_1.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

  Flog.enabled = false

  behavior of "double dummy analysis"
  // NOTE: there is another copy of this test in the functional specs. It currently takes 19 seconds to run this test.
  ignore should "analyze deal 16" in {
    val game = pbn(15)
    println(game)
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


