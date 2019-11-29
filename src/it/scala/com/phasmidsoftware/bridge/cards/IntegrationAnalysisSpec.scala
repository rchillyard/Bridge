/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.pbn.{DealValue, Game, PBN, PBNParser}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

//noinspection ScalaStyle
class IntegrationAnalysisSpec extends FlatSpec with Matchers {

  private val so = Option(getClass.getResourceAsStream("westwood_20190625_1.pbn")) map (Source.fromInputStream(_))
  private val py: Option[PBN] = for (s <- so; p <- PBNParser.parsePBN(s).toOption) yield p
  private val pbn: PBN = py.get

  behavior of "double dummy analysis"
  it should "analyze all deals" in {
    for (game <- pbn) analyzeMakableContracts(game)
  }

  private def analyzeMakableContracts(game: Game): Unit = {
    val board: Int = game("Board").value.toInt
    // FIXME for now, we cannot analyze board 16 :(
    if (board != 16) {
      val event: String = game("Event").value.asString
      val deal = game("Deal").value.asInstanceOf[DealValue].deal
      val detail = game("OptimumResultTable").detail
      val ntContracts = detail.filter(_.contains("NT"))
      val declarerTricksR = """([NESW])\s*NT\s*(\d+)""".r
      ntContracts foreach {
        case declarerTricksR(l, n) =>
          val declarer = "NESW".indexOf(l)
          val leader = Hand.next(declarer)
          val tricks = n.toInt
          print(s"analyzeDoubleDummy: $event Board $board tricks=$tricks, declarer=$l, leader=$leader...")
          System.out.flush()
          val result = Whist(deal, leader).analyzeDoubleDummy(tricks, directionNS = declarer % 2 == 0)
          result match {
            case Some(ok) =>
              if (ok) {
                println(" OK")
                ok shouldBe true
              }
              else fail("finished but wrong answer")
            case None => fail("finished but no result")
          }
      }
    }
  }
}


