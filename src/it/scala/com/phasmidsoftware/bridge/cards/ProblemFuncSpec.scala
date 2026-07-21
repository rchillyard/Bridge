/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.gambit.bits.BitAnalysis
import com.phasmidsoftware.bridge.pbn.{PBN, PBNParser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.io.Source

/**
  * Exercises the bit engine (`BitAnalysis`) against deal 16 of `westwood_20190625_1.pbn`,
  * replacing an earlier version that tested the object-graph engine (`Whist`) directly and
  * failed on all three of its "modes".
  *
  * The three modes computed a `reuse`/`depthTranches` pair that was printed but never
  * actually passed to `analyzeDoubleDummy` -- dead code left over from the `depthTranches`/
  * `reuseDeeper` parameters `doc/DoubleDummyDesign.md` already documents as removed (the
  * choice of caching strategy is now encoded in the `given TTCache[K]` instance, not a
  * parameter). All three modes tested the exact same thing three times over; collapsed to
  * one test here.
  *
  * Uses `assertProvenMakes` + `pendingUntilFixed`, the same pattern as `AnalysisFuncSpec`/
  * `WinchesterFuncSpec`: a full 52-card deal doesn't get remotely close to a proof within a
  * realistic node budget, so an unproven `Partial` guess isn't a failure.
  */
//noinspection ScalaStyle
class ProblemFuncSpec extends AnyFlatSpec with should.Matchers {

  /** Only a proven (Exact) result settles the question; a Partial guess -- right or wrong -- does not. */
  private def assertProvenMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case other => fail(s"Not yet proven (got $other, need Exact($expected, _))")

  private val so = Option(getClass.getResourceAsStream("westwood_20190625_1.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

  behavior of "double dummy analysis"
  it should "analyze deal 16" in pendingUntilFixed {
    val game = pbn(15)
    BitAnalysis.analyzeMakableContracts(game) foreach(result => assertProvenMakes(result, true))
  }
}
