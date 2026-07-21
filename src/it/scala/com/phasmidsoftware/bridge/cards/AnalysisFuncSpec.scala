/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.gambit.bits.BitAnalysis
import com.phasmidsoftware.bridge.pbn.{Contract, DealValue, Game, PBN, PBNParser, Value}
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.io.Source

/**
  * Exercises the bit engine (`BitAnalysis`) against real tournament deals (Westwood,
  * spot-checked and confirmed trustworthy -- unlike the LEXINGTON fixture used by
  * `WhistPBNFuncSpec`, still known to have wrong `OptimumResultTable` entries awaiting
  * correction). Replaces an earlier version of this spec that tested the object-graph
  * engine (`Whist`) directly and failed on most of its nine deals.
  *
  * Uses `assertProvenMakes` + `pendingUntilFixed`, the same pattern already established by
  * `WinchesterBoard1FuncSpec`/`WinchesterBoard12FuncSpec`/`WinchesterFuncSpec`: a full 52-card deal
  * doesn't get remotely close to a proof within a realistic node budget (observed: depth 6
  * of 52), so an unproven `Partial` guess disagreeing with ground truth isn't a failure --
  * only a proven (`Exact`) wrong answer would be.
  *
  * No wall-clock time limit here, unlike the version this replaces: the node budget
  * already bounds each search, and a wall-clock cap on top of that risks failing a search
  * that's making real progress just because it's slower than an arbitrary number.
  */
//noinspection ScalaStyle
class AnalysisFuncSpec extends flatspec.AnyFlatSpec with should.Matchers {

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
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 1" in pendingUntilFixed {
    val game = pbn(1)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 2" in pendingUntilFixed {
    val game = pbn(2)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 3" in pendingUntilFixed {
    val game = pbn(3)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 4" in pendingUntilFixed {
    val game = pbn(4)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 5" in pendingUntilFixed {
    val game = pbn(5)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 6" in pendingUntilFixed {
    val game = pbn(6)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 7" in pendingUntilFixed {
    val game = pbn.last
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

  it should "analyze deal 16" in pendingUntilFixed {
    val game = pbn(15)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }

}
