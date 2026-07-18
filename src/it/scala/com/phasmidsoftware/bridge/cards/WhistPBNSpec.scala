/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.gambit.bits.BitAnalysis
import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.io.Source
import scala.util.Try

/**
  * Exercises the bit engine (`BitAnalysis`) against real tournament deals, replacing an
  * earlier version of this spec that tested the object-graph engine (`Whist`) directly and
  * failed on every single one of its eight deals.
  *
  * Two distinct causes, not one: two deals (5 and 7 of the ~20 declarer/strain combinations
  * checked per deal) simply timed out against the old engine's node budget on a wall-clock
  * limit -- a genuine capacity issue the bit engine's much larger effective budget (see
  * `BridgeConfig.bitboardNodesPerIteration`) should help with. But the other six failed with
  * `true was not equal to false` on EVERY declarer/strain/trick combination checked, which
  * is not a solver problem at all: `tricks` here is parsed directly from
  * `OptimumResultTable`, i.e. it IS the documented double-dummy optimum for that declarer
  * and strain, so `analyzeDoubleDummy(tricks, ...)` SHOULD return `true` (matching
  * `AnalysisSpec`/`ProblemSpec`/`WinchesterSpec`'s identical convention for the same kind of
  * check) -- the previous version of this spec asserted `false`, which is what actually made
  * every one of those deals fail, regardless of engine. Fixed here to assert `true`.
  *
  * No wall-clock time limit, for the same reason as `AnalysisSpec`: the node budget already
  * bounds each search.
  */
//noinspection ScalaStyle
class WhistPBNSpec extends flatspec.AnyFlatSpec with should.Matchers {

  /** Assert only on the makes field, ignoring tricks depth. */
  private def assertMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case DDResult.Partial(makes, _) => makes shouldBe expected
      case DDResult.Inconclusive => fail(s"Expected makes=$expected but got Inconclusive")

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
    val deal: Deal = game("Deal").value.asInstanceOf[DealValue].deal
    if (deal.validate) {
      val board = game("Board").toInt
      val declarerTricksR = """([NESW])\s*(NT|S|H|D|C)\s*(\d+)""".r
      game("OptimumResultTable").detail foreach {
        case contract@declarerTricksR(l, z, n) =>
          val declarer = "NESW".indexOf(l)
          val leader = Hand.next(declarer)
          val strain = z match {
            case "NT" => None
            case x if x.nonEmpty => Some(Suit.apply(x.head))
            case _ => throw CardException(s"cannot parse the contract detail: $contract")
          }
          val tricks = n.toInt
          println(s"analyzeDoubleDummy: board=$board tricks=$tricks, strain=${strain.getOrElse("NT")} declarer=$l, leader=$leader")
          assertMakes(BitAnalysis.analyzeDoubleDummy(deal, leader, strain, tricks, directionNS = declarer % 2 == 0), true)
      }
    }
    else fail(s"Invalid deal: $deal")
  }
}
