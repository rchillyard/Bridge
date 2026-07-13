/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec
import org.scalatest.matchers.should
import org.scalatest.time.{Seconds, Span}

import scala.io.Source

/**
  * Quick spot-check against a real club-session PBN (Winchester Bridge Club, 2026.07.09),
  * whose `OptimumResultTable` entries were computed by a real double-dummy solver
  * (bridgewebs.com), not by this codebase. Deliberately narrow in scope (first three
  * boards, South declaring NT only, mirroring [[AnalysisSpec]]'s filter) to get a fast
  * read on agreement without the long runtimes seen on the westwood/LEXINGTON fixtures.
  */
//noinspection ScalaStyle
class WinchesterSpec extends flatspec.AnyFlatSpec with should.Matchers with TimeLimitedTests {

  val timeLimit = Span(90, Seconds)

  /** Assert only on the makes field, ignoring tricks depth. */
  private def assertMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case DDResult.Partial(makes, _) => makes shouldBe expected
      case DDResult.Inconclusive => fail(s"Expected makes=$expected but got Inconclusive")

  private val so = Option(getClass.getResourceAsStream("/WinchesterJuly2026.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

  behavior of "double dummy analysis against Winchester club results"

  it should "analyze board 1" in {
    analyzeMakableContracts(pbn.head)
  }

  it should "analyze board 2" in {
    analyzeMakableContracts(pbn(1))
  }

  it should "analyze board 3" in {
    analyzeMakableContracts(pbn(2))
  }

  it should "analyze board 7" in {
    analyzeMakableContracts(pbn(6))
  }

  it should "analyze board 12" in {
    analyzeMakableContracts(pbn(11))
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
        assertMakes(Whist(deal, leader).analyzeDoubleDummy(tricks, directionNS = declarer % 2 == 0), true)
    }
  }
}
