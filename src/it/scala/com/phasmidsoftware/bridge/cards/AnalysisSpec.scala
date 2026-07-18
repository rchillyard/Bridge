/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.gambit.bits.BitAnalysis
import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.io.Source

/**
  * Exercises the bit engine (`BitAnalysis`) against real tournament deals, replacing an
  * earlier version of this spec that tested the object-graph engine (`Whist`) directly.
  * That version failed on most of its nine deals: some because the node budget ran out
  * before the search could prove an answer (the old engine's `Partial` best-guess landed
  * on the wrong side), which is exactly the kind of case the bit engine's much larger
  * effective budget (see `BridgeConfig.bitboardNodesPerIteration`) should do better on.
  *
  * No wall-clock time limit here, unlike the version this replaces: the node budget
  * already bounds each search, and a wall-clock cap on top of that risks failing a search
  * that's making real progress just because it's slower than an arbitrary number, the same
  * reasoning already applied to `WinchesterBoard1Spec`/`WinchesterBoard12Spec`.
  */
//noinspection ScalaStyle
class AnalysisSpec extends flatspec.AnyFlatSpec with should.Matchers {

  /** Assert only on the makes field, ignoring tricks depth. */
  private def assertMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case DDResult.Partial(makes, _) => makes shouldBe expected
      case DDResult.Inconclusive => fail(s"Expected makes=$expected but got Inconclusive")

  private val so = Option(getClass.getResourceAsStream("westwood_20190625_1.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

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
        assertMakes(BitAnalysis.analyzeDoubleDummy(deal, leader, None, tricks, directionNS = declarer % 2 == 0), true)
    }
  }
}
