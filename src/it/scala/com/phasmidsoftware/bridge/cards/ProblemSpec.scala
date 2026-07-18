/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.gambit.bits.BitAnalysis
import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
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
  */
//noinspection ScalaStyle
class ProblemSpec extends AnyFlatSpec with should.Matchers {

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
  it should "analyze deal 16" in {
    val game = pbn(15)
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
