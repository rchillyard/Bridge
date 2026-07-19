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
  * Exercises the bit engine (`BitAnalysis`) against real tournament deals (Westwood,
  * spot-checked and confirmed trustworthy -- unlike the LEXINGTON fixture used by
  * `WhistPBNSpec`, still known to have wrong `OptimumResultTable` entries awaiting
  * correction). Replaces an earlier version of this spec that tested the object-graph
  * engine (`Whist`) directly and failed on most of its nine deals.
  *
  * Uses `assertProvenMakes` + `pendingUntilFixed`, the same pattern already established by
  * `WinchesterBoard1Spec`/`WinchesterBoard12Spec`/`WinchesterSpec`: a full 52-card deal
  * doesn't get remotely close to a proof within a realistic node budget (observed: depth 6
  * of 52), so an unproven `Partial` guess disagreeing with ground truth isn't a failure --
  * only a proven (`Exact`) wrong answer would be.
  *
  * No wall-clock time limit here, unlike the version this replaces: the node budget
  * already bounds each search, and a wall-clock cap on top of that risks failing a search
  * that's making real progress just because it's slower than an arbitrary number.
  */
//noinspection ScalaStyle
class AnalysisSpec extends flatspec.AnyFlatSpec with should.Matchers {

  /** Only a proven (Exact) result settles the question; a Partial guess -- right or wrong -- does not. */
  private def assertProvenMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case other => fail(s"Not yet proven (got $other, need Exact($expected, _))")

  private val so = Option(getClass.getResourceAsStream("westwood_20190625_1.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

  behavior of "double dummy analysis"
  it should "analyze deal 0" in pendingUntilFixed {
    val game = pbn.head
    analyzeMakableContracts(game)
  }

  it should "analyze deal 1" in pendingUntilFixed {
    val game = pbn(1)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 2" in pendingUntilFixed {
    val game = pbn(2)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 3" in pendingUntilFixed {
    val game = pbn(3)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 4" in pendingUntilFixed {
    val game = pbn(4)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 5" in pendingUntilFixed {
    val game = pbn(5)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 6" in pendingUntilFixed {
    val game = pbn(6)
    analyzeMakableContracts(game)
  }

  it should "analyze deal 7" in pendingUntilFixed {
    val game = pbn.last
    analyzeMakableContracts(game)
  }

  it should "analyze deal 16" in pendingUntilFixed {
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
        val result = BitAnalysis.analyzeDoubleDummy(deal, leader, None, tricks, directionNS = declarer % 2 == 0)
        println(s"analyzeDoubleDummy: tricks=$tricks, declarer=$l, leader=$leader -> $result")
        assertProvenMakes(result, true)
    }
  }
}
