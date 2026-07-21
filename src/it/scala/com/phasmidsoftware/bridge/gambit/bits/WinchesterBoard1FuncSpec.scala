package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.*
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * Board 1 of `WinchesterJuly2026.pbn`
  * (`[Deal "N:J3.975.QT764.AJ5 KQ62.JT2.KJ3.832 T94.A64.95.T9764 A875.KQ83.A82.KQ"]`):
  *
  * its `OptimumResultTable` says `N NT 1` / `S NT 1` (NS makes only 1 trick in NT), but Robin's
  * spot check against an online double-dummy solver said 2 -- and that's the more plausible
  * answer on its face: NS holds two lone aces (North's club ace, South's heart ace), and a lone
  * ace is essentially an unstoppable notrump winner absent a squeeze. This looked like a likely
  * error in the PBN fixture itself, not a solver disagreement.
  *
  * **Update, 2026-07-18**: the new bitboard engine now PROVES it. With the rank-reduction
  * `SuitMask.compactor` fix landed (see `doc/DoubleDummyDesign.md`'s "Not Yet Implemented" #1),
  * this full 52-card deal resolves to `Exact(true,13)` for "NS makes 2 tricks in NT" under the
  * project's normal (default) settings -- confirming Robin's spot check over the fixture's
  * claimed "1". The fixture's `OptimumResultTable` entry itself is Robin's own call to correct,
  * not this file's -- left as-is here since this spec only tests what the solver can prove, not
  * the PBN file's contents.
  *
  * The complementary "NOT 3 tricks" question is NOT yet provable at the project's shipped
  * settings (`useCanonicalKey` defaults to `false`; the new engine still returns an unproven
  * `Partial(false,7)` here) -- that assertion stays `pendingUntilFixed`.
  *
  * **Update, 2026-07-21**: dropped the two old-engine (`Whist`) tests that used to live here --
  * both were `pendingUntilFixed` and never got further than 5 tricks deep on this full 52-card
  * deal before running out of node budget, so they were never proving anything.
  */
//noinspection ScalaStyle
class WinchesterBoard1FuncSpec extends flatspec.AnyFlatSpec with should.Matchers {

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

  it should "have NS proven to make 2 tricks in NT, per the NEW bitboard engine" in {
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
