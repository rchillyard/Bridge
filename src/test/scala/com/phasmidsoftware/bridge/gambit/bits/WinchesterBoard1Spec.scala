package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.*
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * Open question on board 1 of `WinchesterJuly2026.pbn`
  * (`[Deal "N:J3.975.QT764.AJ5 KQ62.JT2.KJ3.832 T94.A64.95.T9764 A875.KQ83.A82.KQ"]`):
  *
  * its `OptimumResultTable` says `N NT 1` / `S NT 1` (NS makes only 1 trick in NT), but Robin's
  * spot check against an online double-dummy solver says 2 -- and that's the more plausible
  * answer on its face: NS holds two lone aces (North's club ace, South's heart ace), and a lone
  * ace is essentially an unstoppable notrump winner absent a squeeze. This looks like a likely
  * error in the PBN fixture itself, not a solver disagreement.
  *
  * Neither engine can currently PROVE this either way: board 1 is a full 52-card deal, and as of
  * this writing neither engine resolves a full deal to `Exact` within the 1,000,000-node-per-
  * iteration budget (old engine reached 5 tricks deep, the new bitboard engine 3, before
  * returning an unproven `Partial` guess) -- see the wider "search doesn't complete on real
  * deals" finding this whole bitboard project is aimed at. `Partial` guesses are known to be
  * unreliable (that's the reason this file exists), so they don't confirm or refute the "2
  * tricks" claim either way.
  *
  * These tests are pending until an engine actually proves (`DDResult.Exact`) whether NS makes
  * 2 tricks in NT here -- at which point `pendingUntilFixed` will fail loudly, as a prompt to
  * replace it with a real assertion.
  */
//noinspection ScalaStyle
class WinchesterBoard1Spec extends flatspec.AnyFlatSpec with should.Matchers {

  private val deal: Deal = Deal.fromHandStrings("test", "N", List(
    List("J3", "975", "QT764", "AJ5"),
    List("KQ62", "JT2", "KJ3", "832"),
    List("T94", "A64", "95", "T9764"),
    List("A875", "KQ83", "A82", "KQ")
  ))

  /** Only a proven (Exact) result settles the question; a Partial guess -- right or wrong -- does not. */
  private def assertProvenMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case other => fail(s"Not yet proven (got $other, need Exact($expected, _))")

  it should "have NS proven to make 2 tricks in NT, per the OLD engine" in pendingUntilFixed {
    val result = Whist(deal, Hand.next(0), None).analyzeDoubleDummy(2, directionNS = true)
    println(s"OLD engine, NS needing 2 tricks in NT: $result")
    assertProvenMakes(result, true)
  }

  it should "have NS proven NOT to make 3 tricks in NT, per the OLD engine" in pendingUntilFixed {
    val result = Whist(deal, Hand.next(0), None).analyzeDoubleDummy(3, directionNS = true)
    println(s"OLD engine, NS needing 3 tricks in NT: $result")
    assertProvenMakes(result, false)
  }

  it should "have NS proven to make 2 tricks in NT, per the NEW bitboard engine" in pendingUntilFixed {
    val result = BitAnalysis.analyzeDoubleDummy(deal, Hand.next(0), None, 2, directionNS = true)
    println(s"NEW engine, NS needing 2 tricks in NT: $result")
    assertProvenMakes(result, true)
  }

  it should "have NS proven NOT to make 3 tricks in NT, per the NEW bitboard engine" in pendingUntilFixed {
    val result = BitAnalysis.analyzeDoubleDummy(deal, Hand.next(0), None, 3, directionNS = true)
    println(s"NEW engine, NS needing 3 tricks in NT: $result")
    assertProvenMakes(result, false)
  }
}
