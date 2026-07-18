package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.{Clubs, DDResult, Deal, Suit, Whist}
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * Validates the new bitboard engine (`BitAnalysis`) against the SAME small, trusted
  * hand-built end positions used in `WhistSpec` to validate the existing object-graph
  * engine -- same deals, same expected answer, both engines exercised independently.
  */
//noinspection ScalaStyle
class BitAnalysisSpec extends flatspec.AnyFlatSpec with should.Matchers {

  private def assertMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case DDResult.Partial(makes, _) => makes shouldBe expected
      case DDResult.Inconclusive => fail(s"Expected makes=$expected but got Inconclusive")

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

  behavior of "BitAnalysis.analyzeDoubleDummy against known small end positions"

  it should "agree on the four-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ", "", "J", "3"), List("K3", "T", "", "6"), List("", "87", "Q", "8"), List("", "A", "9", "T9")))
    assertMakes(BitAnalysis.analyzeDoubleDummy(target, 3, Some(Clubs), 3, directionNS = true), false)
  }

  it should "agree on the five-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ", "9", "J", "3"), List("K32", "T", "", "6"), List("4", "87", "Q", "8"), List("5", "A", "9", "T9")))
    val tricks = target.nCards / Deal.CardsPerTrick
    assertMakes(BitAnalysis.analyzeDoubleDummy(target, 3, Some(Clubs), tricks, directionNS = true), false)
  }

  it should "agree on the six-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ6", "9", "J", "3"), List("K32", "T", "T", "6"), List("4", "87", "Q", "87"), List("5", "AK", "9", "T9")))
    val tricks = target.nCards / Deal.CardsPerTrick
    assertMakes(BitAnalysis.analyzeDoubleDummy(target, 3, Some(Clubs), tricks, directionNS = true), false)
  }

  it should "agree on the seven-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "3"), List("K32", "QT", "T", "6"), List("4", "87", "Q", "874"), List("5", "AK", "9", "T95")))
    val tricks = target.nCards / Deal.CardsPerTrick
    assertMakes(BitAnalysis.analyzeDoubleDummy(target, 3, Some(Clubs), tricks, directionNS = true), false)
  }

  it should "agree on the eight-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "32"), List("K32", "QT", "T", "J6"), List("4", "87", "Q", "Q874"), List("5", "AK", "9", "KT95")))
    val tricks = target.nCards / Deal.CardsPerTrick
    assertMakes(BitAnalysis.analyzeDoubleDummy(target, 3, Some(Clubs), tricks, directionNS = true), false)
  }

  it should "agree on the nine-card end position" in {
    val target = Deal.fromHandStrings("test", "N", List(List("AQJ76", "9", "J", "32"), List("K32", "QJT", "T", "J6"), List("4", "87", "Q8", "Q874"), List("5", "AK", "97", "KT95")))
    val tricks = target.nCards / Deal.CardsPerTrick
    assertMakes(BitAnalysis.analyzeDoubleDummy(target, 3, Some(Clubs), tricks, directionNS = true), false)
  }

  // All six positions above only exercise a target (`neededTricks`) known in advance to be
  // false. To rule out a degenerate engine that just always says "false" regardless of the
  // actual cards, cross-check against the trusted old engine's own live answer across every
  // possible target on the four-card end position -- this necessarily includes targets that
  // resolve `true` (e.g. neededTricks=1, an easy bar NS should clear given they hold the
  // Spade AQ), without requiring a hand-derived ground truth for each one.
  behavior of "BitAnalysis vs the object-graph engine, head-to-head"

  it should "agree with Whist.analyzeDoubleDummy across every target on the four-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ", "", "J", "3"), List("K3", "T", "", "6"), List("", "87", "Q", "8"), List("", "A", "9", "T9"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the five-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ", "9", "J", "3"), List("K32", "T", "", "6"), List("4", "87", "Q", "8"), List("5", "A", "9", "T9"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the six-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ6", "9", "J", "3"), List("K32", "T", "T", "6"), List("4", "87", "Q", "87"), List("5", "AK", "9", "T9"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the seven-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "3"), List("K32", "QT", "T", "6"), List("4", "87", "Q", "874"), List("5", "AK", "9", "T95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the eight-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "32"), List("K32", "QT", "T", "J6"), List("4", "87", "Q", "Q874"), List("5", "AK", "9", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the nine-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQJ76", "9", "J", "32"), List("K32", "QJT", "T", "J6"), List("4", "87", "Q8", "Q874"), List("5", "AK", "97", "KT95"))))
  }

  // The ten/eleven/twelve/thirteen-card cross-checks moved to BitAnalysisITSpec (src/it):
  // full-budget searches on these larger end positions are slow enough (minutes, one
  // observed IDE run OOM'd) that they don't belong in the default `sbt test` run.

  // The three-card "automatic squeeze" position from WhistSpec -- the exact deal that exposed
  // the evaluateKey collision bug in the object-graph engine (a proven-makeable squeeze coming
  // back as not-makeable once LowerBound/UpperBound TT reuse was added). Both engines' TTs get
  // exercised hardest here, across all four leaders and both strains, since that's exactly the
  // dimension (trick-in-progress state, NS/EW split) the bug was in.
  it should "agree with Whist.analyzeDoubleDummy on the three-card automatic squeeze, every leader and strain" in {
    val target = Deal.fromHandStrings("test", "N",
      List(List("AJ", "K", "", ""), List("KQ", "A", "", ""), List("2", "2", "", "A"), List("98", "", "4", "")))
    for (leader <- 0 to 3; strain <- Seq(Some(Clubs), None)) {
      val oldResult = Whist(target, leader, strain).analyzeDoubleDummy(3, directionNS = true)
      val newResult = BitAnalysis.analyzeDoubleDummy(target, leader, strain, 3, directionNS = true)
      withClue(s"leader=$leader, strain=$strain: old=$oldResult, new=$newResult: ") {
        makesOf(newResult) shouldBe makesOf(oldResult)
      }
    }
  }
}
