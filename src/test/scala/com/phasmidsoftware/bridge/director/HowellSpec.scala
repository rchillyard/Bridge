/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director
import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  * @author robinhillyard
  */

class HowellSpec extends FlatSpec with Matchers with Inside {
  "modulo" should "work on 1-n, etc." in {
    implicit val tables: Int = 4
    val e = Encounter(1, 2, 3, 4)
    e.modulo(-3) shouldBe 1
    e.modulo(-2) shouldBe 2
    e.modulo(2) shouldBe 2
    e.modulo(-1) shouldBe 3
    e.modulo(0) shouldBe 4
  }
  "move" should "work on Encounter" in {
    implicit val tables: Int = 4
    val e = Encounter(1, 2, 3, 4)
    val p = Position(Seq(Encounter(1, 3, 2, 1), Encounter(2, 1, 4, 2), Encounter(3, 4, 1, 3), Encounter(4, 2, 3, 4)))
    val moves = director.Triple(-3, -2, -1)
    val x = e.move(moves, p)
    x shouldBe Encounter(1, 2, 1, 2)
  }

  "Howell" should "work with 4 tables" in {
    implicit val tables: Int = 4
    val howell = Howell(tables, director.Triple(Seq(-3), Seq(-2), Seq(-1)))
    val start = Position(Seq(Encounter(1, 3, 2, 1), Encounter(2, 1, 4, 2), Encounter(3, 4, 1, 3), Encounter(4, 2, 3, 4)))
    val positions = howell.positions(start)
    positions.size shouldBe 4
    positions.tail.head shouldBe Position(Seq(Encounter(1, 2, 1, 2), Encounter(2, 3, 3, 3), Encounter(3, 1, 2, 4), Encounter(4, 4, 4, 1)))
    val labeledPositions = Howell.labelPositions(howell.positions(start))
    labeledPositions.tail.tail.head should matchPattern {
      case (3, Position(Seq(Encounter(1, 4, 2, 3), Encounter(2, 2, 4, 4), Encounter(3, 3, 1, 1), Encounter(4, 1, 3, 2)))) =>
    }
  }

  it should "work with 7 tables" in {
    implicit val tables: Int = 7
    val howell = director.Howell(tables, director.Triple(Seq(-3), Seq(-2), Seq(-1)))
    val start = Position(Seq(Encounter(1, 1, 1, 1), Encounter(2, 6, 5, 2), Encounter(3, 4, 2, 3), Encounter(4, 2, 6, 4), Encounter(5, 7, 3, 5), Encounter(6, 5, 7, 6), Encounter(7, 3, 4, 7)))
    val positions = howell.positions(start)
    positions.size shouldBe 7
    positions.tail.head shouldBe Position(Seq(Encounter(1, 2, 2, 2), Encounter(2, 7, 6, 3), Encounter(3, 5, 3, 4), Encounter(4, 3, 7, 5), Encounter(5, 1, 4, 6), Encounter(6, 6, 1, 7), Encounter(7, 4, 5, 1)))
    val labeledPositions = Howell.labelPositions(howell.positions(start))
    for ((r, p) <- labeledPositions)
      println(s"Round $r: $p")

    //    labeledPositions.tail.tail.head should matchPattern { case (3,Position(Seq(Encounter(1,4,2,3),Encounter(2,2,4,4),Encounter(3,3,1,1),Encounter(4,1,3,2)))) => }
  }

  "Triple.zip" should "work" in {
    val z = director.Triple(Stream.from(1), Stream.from(2), Stream.from(3))
    val stream = director.Triple.zip(z)
    stream.head shouldBe director.Triple(1, 2, 3)
    stream.tail.head shouldBe director.Triple(2, 3, 4)
  }
  "Triple.toStreams" should "work" in {
    val ist = director.Triple(Seq(1), Seq(2), Seq(1, 2))
    val streams = director.Triple.toStreams(ist)
    streams._1.head shouldBe 1
    streams._2.head shouldBe 2
    streams._3.head shouldBe 1
    streams._1.tail.head shouldBe 1
    streams._2.tail.head shouldBe 2
    streams._3.tail.head shouldBe 2
  }
}
  