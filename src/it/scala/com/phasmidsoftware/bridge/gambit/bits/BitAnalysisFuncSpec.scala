package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.SlowTest
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
  * **Update, 2026-07-19**: all three of ten, eleven, and twelve cards were originally known,
  * real disagreements here, blamed on the new engine's lack of Strategy-based move ordering.
  * That diagnosis was only partly right (see `doc/DoubleDummyDesign.md`'s "Known Open Gap"):
  * move ordering fixed the ten-card case outright, and the eleven-card case turned out to be
  * a transposition-table size / node-budget problem, since resolved by the bit engine's own
  * (larger) TT/budget tuning. The twelve-card case remained open longer still, needing a
  * bigger TT/node budget than was judged worth the memory margin -- a deliberate, accepted
  * tradeoff, not an unexplained gap.
  *
  * **Update, 2026-07-20**: the twelve-card case is now resolved too, at the shipped default
  * budget, no bigger TT/margin needed -- the opening-lead priority scale added to
  * `BitState.leadScore` (see `doc/DoubleDummyDesign.md`'s "Move Ordering") closed it.
  *
  * **Update, 2026-07-21**: also absorbed the seven/eight/nine-card cross-checks from
  * `BitAnalysisSpec` (src/test), for a DIFFERENT reason than the ten-thirteen-card ones above --
  * these still run in a couple of seconds, but the seven-card version hung a real CircleCI job
  * for 10+ minutes with no console output before being killed, apparently from the old engine's
  * per-node cost colliding badly with the container's memory limit (see `.circleci/config.yml`
  * and `build.sbt`'s `forkedHeapXmx` for the actual fix). Moving them here means they simply
  * don't run in CircleCI at all (nothing currently invokes `IT` there), rather than depending on
  * a `Slow` tag that would still expose the same job to the same collision later.
  */
//noinspection ScalaStyle
class BitAnalysisFuncSpec extends flatspec.AnyFlatSpec with should.Matchers {

  private def makesOf(result: DDResult): Boolean = result match
    case DDResult.Exact(makes, _) => makes
    case DDResult.Partial(makes, _) => makes
    case DDResult.Inconclusive => fail(s"Unexpected Inconclusive: $result")

  /** Cross-checks every target from 1 up to the deal's own trick count against the trusted
    * old engine's own live answer, rather than a single hand-derived expected value. */
  private def crossCheckEveryTarget(target: Deal, leader: Int = 3, strain: Option[Suit] = Some(Clubs), useCanonicalKey: Boolean = false): Unit =
    val maxTricks = target.nCards / Deal.CardsPerTrick
    for (neededTricks <- 1 to maxTricks) {
      val oldResult = Whist(target, leader, strain).analyzeDoubleDummy(neededTricks, directionNS = true)
      val newResult = BitAnalysis.analyzeDoubleDummy(target, leader, strain, neededTricks, directionNS = true, useCanonicalKey = useCanonicalKey)
      withClue(s"(clue): neededTricks=$neededTricks: old=$oldResult, new=$newResult: ") {
        makesOf(newResult) shouldBe makesOf(oldResult)
      }
    }

  // Moved from BitAnalysisSpec (src/test) -- the seven-card version is a confirmed CircleCI
  // hang risk (see the class doc comment's 2026-07-21 update), and eight/nine only go deeper.
  // Measured locally: seven/eight-card stay under 7s either way; nine-card actually crosses
  // this project's own ~30s Slow threshold (33.8s / 29.8s canonical), tagged accordingly.
  behavior of "BitAnalysis vs the object-graph engine, head-to-head, small-but-CI-risky end positions"

  it should "agree with Whist.analyzeDoubleDummy across every target on the seven-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "3"), List("K32", "QT", "T", "6"), List("4", "87", "Q", "874"), List("5", "AK", "9", "T95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the eight-card end position" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "32"), List("K32", "QT", "T", "J6"), List("4", "87", "Q", "Q874"), List("5", "AK", "9", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the nine-card end position" taggedAs SlowTest in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQJ76", "9", "J", "32"), List("K32", "QJT", "T", "J6"), List("4", "87", "Q8", "Q874"), List("5", "AK", "97", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the seven-card end position (canonical key)" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "3"), List("K32", "QT", "T", "6"), List("4", "87", "Q", "874"), List("5", "AK", "9", "T95"))), useCanonicalKey = true)
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the eight-card end position (canonical key)" in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQ76", "9", "J", "32"), List("K32", "QT", "T", "J6"), List("4", "87", "Q", "Q874"), List("5", "AK", "9", "KT95"))), useCanonicalKey = true)
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the nine-card end position (canonical key)" taggedAs SlowTest in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(List("AQJ76", "9", "J", "32"), List("K32", "QJT", "T", "J6"), List("4", "87", "Q8", "Q874"), List("5", "AK", "97", "KT95"))), useCanonicalKey = true)
  }

  behavior of "BitAnalysis vs the object-graph engine, head-to-head, larger end positions"

  it should "agree with Whist.analyzeDoubleDummy across every target on the ten-card end position" taggedAs SlowTest in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ76", "96", "J", "32"),
      List("K32", "QJT5", "T", "J6"),
      List("4", "874", "Q8", "Q874"),
      List("5", "AK3", "97", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the eleven-card end position" taggedAs SlowTest in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ76", "96", "AJ", "32"),
      List("KT32", "QJT5", "T", "J6"),
      List("4", "8742", "Q8", "Q874"),
      List("95", "AK3", "97", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the twelve-card end position" taggedAs SlowTest in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ876", "96", "AJ", "32"),
      List("KT32", "QJT5", "KT", "J6"),
      List("4", "8742", "Q86", "Q874"),
      List("95", "AK3", "975", "KT95"))))
  }

  it should "agree with Whist.analyzeDoubleDummy across every target on the thirteen-card end position" taggedAs SlowTest in {
    crossCheckEveryTarget(Deal.fromHandStrings("test", "N", List(
      List("AQJ876", "96", "AJ4", "32"),
      List("KT32", "QJT5", "KT3", "J6"),
      List("4", "8742", "Q862", "Q874"),
      List("95", "AK3", "975", "AKT95"))))
  }
}
