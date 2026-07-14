package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.{Clubs, DDResult, Deal, Suit, Whist}
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * The larger head-to-head cross-checks split off `BitAnalysisSpec` (src/test): each
  * `crossCheckEveryTarget` call here runs full-budget searches on BOTH engines for every
  * target from 1 up to the deal's own trick count, and on a ten-to-thirteen-card end
  * position that's expensive enough (minutes; one IDE run against the ten-card position
  * ran out of memory) that it doesn't belong in the default `sbt test` run.
  *
  * Three of these (ten, eleven, twelve cards) are known, real disagreements, not yet root
  * caused -- the new engine converges to less depth than the old engine on the same
  * position and target (suspected cause: the new engine's lack of Strategy-based move
  * ordering, a documented simplification of the bitboard rewrite). Left as plain failing
  * assertions rather than `pendingUntilFixed`/`ignore`, so `sbt IT/test` reports them
  * honestly.
  */
//noinspection ScalaStyle
class BitAnalysisITSpec extends flatspec.AnyFlatSpec with should.Matchers {

  private def makesOf(result: DDResult): Boolean = result match
    case DDResult.Exact(makes, _) => makes
    case DDResult.Partial(makes, _) => makes
    case DDResult.Inconclusive => fail(s"Unexpected Inconclusive: $result")

  /** Cross-checks every target from 1 up to the deal's own trick count against the trusted
    * old engine's own live answer, rather than a single hand-derived expected value. */
  private def crossCheckEveryTarget(target: Deal, leader: Int = 3, strain: Option[Suit] = Some(Clubs)): Unit =
    val maxTricks = target.nCards / Deal.CardsPerTrick
    for (neededTricks <- 1 to maxTricks) {
      val oldResult = Whist(target, leader, strain).analyzeDoubleDummy(neededTricks, directionNS = true)
      val newResult = BitAnalysis.analyzeDoubleDummy(target, leader, strain, neededTricks, directionNS = true)
      withClue(s"neededTricks=$neededTricks: old=$oldResult, new=$newResult: ") {
        makesOf(newResult) shouldBe makesOf(oldResult)
      }
    }

  behavior of "BitAnalysis vs the object-graph engine, head-to-head, larger end positions"

  it should "agree with Whist.analyzeDoubleDummy across every target on the ten-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ76", "96", "J", "32"),
      List("K32", "QJT5", "T", "J6"),
      List("4", "874", "Q8", "Q874"),
      List("5", "AK3", "97", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the eleven-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ76", "96", "AJ", "32"),
      List("KT32", "QJT5", "T", "J6"),
      List("4", "8742", "Q8", "Q874"),
      List("95", "AK3", "97", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the twelve-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ876", "96", "AJ", "32"),
      List("KT32", "QJT5", "KT", "J6"),
      List("4", "8742", "Q86", "Q874"),
      List("95", "AK3", "975", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the thirteen-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ876", "96", "AJ4", "32"),
      List("KT32", "QJT5", "KT3", "J6"),
      List("4", "8742", "Q862", "Q874"),
      List("95", "AK3", "975", "AKT95"))))
  }
}
