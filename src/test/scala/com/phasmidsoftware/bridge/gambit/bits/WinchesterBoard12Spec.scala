package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.*
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * Board 12 of `WinchesterJuly2026.pbn`
  * (`[Deal "W:86.KQ7.Q8754.Q95 9742.JT65.32.A72 KQT5.9843.T6.864 AJ3.A2.AKJ9.KJT3"]`):
  * its `OptimumResultTable` says `S NT 9`. Unlike board 1, there's no reason yet to doubt
  * this figure -- this file is testing whether either engine can actually PROVE it, not
  * whether the figure itself is right.
  *
  * Same `Exact`-only structure as `WinchesterBoard1Spec`, but exercising `analyzeDoubleDummy`'s
  * `maxNodes` override with a bigger-than-default budget, on Robin's suspicion (from watching
  * this board run manually) that it might resolve with more patience.
  *
  * The old and new engines get DIFFERENT budgets here, and that's deliberate, not an oversight:
  * at 5x the default (5,000,000), the old engine reliably OOMs (observed: 13 minutes, heap
  * climbing steadily to the -Xmx8g ceiling, dead at 2.6M nodes into a single iteration -- its
  * per-node cost is the whole reason this bitboard project exists). The new engine handles the
  * same 5x budget in ~14 seconds with heap never exceeding ~2.6GB and clear GC reclamation
  * between checkpoints -- a real, measured confirmation (not just a design argument) that it
  * isn't retaining `Deal`/`Hand`/`Holding` objects the way the old engine does. The old engine's
  * tests here stay at the safe default so this file doesn't turn every `sbt test` into a
  * 13-minute crash.
  */
//noinspection ScalaStyle
class WinchesterBoard12Spec extends flatspec.AnyFlatSpec with should.Matchers {

  private val oldEngineMaxNodes = BridgeConfig.nodesPerIteration // 5x reliably OOMs -- see class doc
  private val newEngineMaxNodes = 5_000_000 // confirmed safe: ~14s, heap under 2.6GB -- see class doc

  // fromHandStrings expects wss in the same rotational order as the PBN string itself (i.e.
  // starting from whichever seat "start" names, going clockwise) -- NOT rearranged to always
  // start with North. Board 12's PBN is "W:86.KQ7.Q8754.Q95 9742.JT65.32.A72 KQT5.9843.T6.864
  // AJ3.A2.AKJ9.KJT3", i.e. West, North, East, South in that literal order.
  private val deal: Deal = Deal.fromHandStrings("test", "W", List(
    List("86", "KQ7", "Q8754", "Q95"), // West
    List("9742", "JT65", "32", "A72"), // North
    List("KQT5", "9843", "T6", "864"), // East
    List("AJ3", "A2", "AKJ9", "KJT3") // South
  ))

  private val declarer = 2 // South
  private val leader = Hand.next(declarer) // West

  /** Only a proven (Exact) result settles the question; a Partial guess -- right or wrong -- does not. */
  private def assertProvenMakes(result: DDResult, expected: Boolean): Unit =
    result match
      case DDResult.Exact(makes, _) => makes shouldBe expected
      case other => fail(s"Not yet proven (got $other, need Exact($expected, _))")

  it should "have NS proven to make 9 tricks in NT, per the OLD engine" in pendingUntilFixed {
    val result = Whist(deal, leader, None).analyzeDoubleDummy(9, directionNS = true, maxNodes = oldEngineMaxNodes)
    println(s"OLD engine, NS needing 9 tricks in NT: $result")
    assertProvenMakes(result, true)
  }

  it should "have NS proven NOT to make 10 tricks in NT, per the OLD engine" in pendingUntilFixed {
    val result = Whist(deal, leader, None).analyzeDoubleDummy(10, directionNS = true, maxNodes = oldEngineMaxNodes)
    println(s"OLD engine, NS needing 10 tricks in NT: $result")
    assertProvenMakes(result, false)
  }

  it should "have NS proven to make 9 tricks in NT, per the NEW bitboard engine" in pendingUntilFixed {
    val result = BitAnalysis.analyzeDoubleDummy(deal, leader, None, 9, directionNS = true, maxNodes = newEngineMaxNodes)
    println(s"NEW engine, NS needing 9 tricks in NT: $result")
    assertProvenMakes(result, true)
  }

  it should "have NS proven NOT to make 10 tricks in NT, per the NEW bitboard engine" in pendingUntilFixed {
    val result = BitAnalysis.analyzeDoubleDummy(deal, leader, None, 10, directionNS = true, maxNodes = newEngineMaxNodes)
    println(s"NEW engine, NS needing 10 tricks in NT: $result")
    assertProvenMakes(result, false)
  }
}
