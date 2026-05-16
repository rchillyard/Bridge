/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source

//noinspection ScalaStyle
class ProblemSpec extends AnyFlatSpec with should.Matchers {

  State.count = 0
  private val so = Option(getClass.getResourceAsStream("westwood_20190625_1.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

  behavior of "double dummy analysis"
  // NOTE: there is another copy of this test in the functional specs.
  //  It currently takes 3.5 seconds to run this test.
  it should "analyze deal 16 mode 0" in {
    val game = pbn(15)
    println(game)
    analyzeMakeableContracts(game, 0)
  }
  it should "analyze deal 16 mode 1" in {
    val game = pbn(15)
    println(game)
    analyzeMakeableContracts(game, 1)
  }
  it should "analyze deal 16 mode 2" in {
    val game = pbn(15)
    println(game)
    analyzeMakeableContracts(game, 2)
  }

  private def analyzeMakeableContracts(game: Game, mode: Int): Unit = {
    val deal = game("Deal").value.asInstanceOf[DealValue].deal
    val detail = game("OptimumResultTable").detail
    val ntContracts = detail.filter(_.contains("NT")).filter(_.startsWith("S"))
    val declarerTricksR = """([NESW])\s*NT\s*(\d+)""".r
    ntContracts foreach {
      case declarerTricksR(l, n) =>
        val declarer = "NESW".indexOf(l)
        val leader = Hand.next(declarer)
        val tricks = n.toInt
        val (reuse, depthT) = mode match {
          case 1 => false -> true
          case 2 => true -> true
          case _ => false -> false
        }
        println(s"analyzeDoubleDummy: tricks=$tricks, declarer=$l, leader=$leader, mode=$mode, reuseDeeper=$reuse, depthTranches=$depthT")
        Whist(deal, leader).analyzeDoubleDummy(tricks, directionNS = declarer % 2 == 0, reuseDeeper = reuse, depthTranches = depthT) shouldBe Some(true)
    }
  }
}


