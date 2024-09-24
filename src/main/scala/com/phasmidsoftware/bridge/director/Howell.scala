/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.Howell.{MovePlan, Moves, Trio}

import scala.annotation.tailrec

/**
  * @author robinhillyard
  */
/**
  * tables is the total number of tables, including phantom tables (should be an odd number).
  * For a complete movement, the number of participating pairs is the number of tables plus one.
  * The number of true (physical) tables is half the number of pairs.
  * The number of board sets is the same as the number of tables.
  * All positions are 0-based
  */
case class Howell(name: String, tables: List[Table], movePlan: MovePlan) {
  def moves(current: Position, movement: Movement): (Position, Movement) = {
    val head = movement.head
    val tail = movement.tail
    val result = for (e <- current.encounters) yield e.move(head, current)
    implicit val _: Int = tables.length
    (Position(result), Movement(tail))
  }

  private def getMovement(implicit tables: Int): Movement = Movement(Triple.toLazyLists(movePlan))

  def positions(start: Position): List[Position] = {
    @tailrec def loop(positions: List[Position], posMov: (Position, Movement), moves: Int): List[Position] = moves match {
      case 0 => positions
      case _ => loop(positions :+ posMov._1, this.moves(posMov._1, posMov._2), moves - 1)
    }

    implicit val t: Int = tables.length
    loop(List(), (start, getMovement), t)
  }

  def showTable(x: Int): String = tables(x - 1).toString
}

case class Position(encounters: List[Encounter]) {
  override def toString: String = encounters mkString " "
}

case class Round(round: Int, position: Position) {
  override def toString: String = s"Round $round: $position"
}
//
//abstract class MappedProduct3[T, F[_]](_1: T, _2: T, _3: T) extends Product3[T, T, T] {
//  def map[U](f: T => U)(implicit cbf: CanBuildFrom[F[U], U, F[U]]): F[U] = {
//    val ts = productIterator
//    val us = for (t <- ts) yield f(t.asInstanceOf[T])
//    val x: mutable.Builder[U, F[U]] = cbf()
//    for (u <- us) x += u
//    x.result()
//  }
//
//  override def toString: String = s"n:${_1} e:${_2} b:${_3}"
//}

case class Triple[T](_1: T, _2: T, _3: T) extends Product3[T, T, T] {
  def map[U](f: T => U): Triple[U] = Triple(f(_1), f(_2), f(_3))

  override def toString: String = s"n:${_1} e:${_2} b:${_3}"
}

object Triple {
  def zip[U](ust: Triple[LazyList[U]]): LazyList[Triple[U]] = ust._1 zip ust._2 zip ust._3 map { case ((x, y), z) => Triple(x, y, z) }

  def toLazyLists[U](ust: Triple[List[U]]): Triple[LazyList[U]] = ust.map(us => LazyList.continually(us).flatten)
}

case class Movement(moves: LazyList[Trio]) {
  def head: Trio = moves.head

  def tail: LazyList[Trio] = moves.tail
}

object Movement {
  def apply(x: Moves)(implicit tables: Int): Movement =
    Movement(Triple.zip(x))
}

case class Table(number: Int, phantom: Boolean) {
  override def toString: String = s"T$number" + (if (phantom) "*" else " ")
}

object Table {
  //  def apply(x: Int): Table = Table(x, phantom = false)
  //  def phantom(x: Int): Table = Table(x, phantom = true)

  def tables(phantoms: Int*)(implicit tables: Int): List[Table] = (for (i <- 1 to tables) yield Table(i, phantoms contains i)) to List
}

//noinspection ScalaStyle
object Howell extends App {
  type Trio = Triple[Int]
  type MovePlan = Triple[List[Int]]
  type Moves = Triple[LazyList[Int]]

  lazy val howell7: (Howell, Position) = {
    implicit val n: Int = 7
    implicit val tables: List[Table] = Table.tables(4, 6, 7)
    val howell = Howell("4-Table", tables, Triple(List(-3), List(-2), List(-1)))
    val start = Position(List(
      Encounter(1, 1, 1, 1),
      Encounter(2, 6, 5, 2),
      Encounter(3, 4, 2, 3),
      Encounter(4, 2, 6, 4),
      Encounter(5, 7, 3, 5),
      Encounter(6, 5, 7, 6),
      Encounter(7, 3, 4, 7)))
    (howell, start)
  }

  lazy val howell9: (Howell, Position) = {
    implicit val n: Int = 9
    implicit val tables: List[Table] = Table.tables(3, 5, 7, 8) // 3 dups
    //    implicit val tables = Table.tables(3, 5, 7, 9) // 2 dups
    //    implicit val tables = Table.tables(3, 5, 6, 8) // 3 dups
    val howell = Howell("5-table", tables, Triple(List(-3, -3, -2), List(-2), List(-1)))
    val start = Position(List(
      Encounter(1, 1, 1, 1),
      Encounter(2, 8, 6, 2),
      Encounter(3, 5, 2, 3),
      Encounter(4, 2, 7, 4),
      Encounter(5, 9, 3, 5),
      Encounter(6, 6, 8, 6),
      Encounter(7, 3, 4, 7),
      Encounter(8, 7, 9, 8),
      Encounter(9, 4, 5, 9)))
    (howell, start)
  }

  lazy val howell9a: (Howell, Position) = {
    implicit val n: Int = 9
    //  implicit val tables = Table.tables(3, 5, 7, 8) // 1 dups
    implicit val tables: List[Table] = Table.tables(3, 5, 6, 8) // 2 dups
    val howell = Howell("5-table", tables, Triple(List(-3, -3, -4), List(-2), List(-1)))
    val start = Position(List(
      Encounter(1, 1, 1, 1),
      Encounter(2, 4, 6, 2),
      Encounter(3, 7, 2, 3),
      Encounter(4, 2, 7, 4),
      Encounter(5, 5, 3, 5),
      Encounter(6, 8, 8, 6),
      Encounter(7, 3, 4, 7),
      Encounter(8, 6, 9, 8),
      Encounter(9, 9, 5, 9)))
    (howell, start)
  }
  lazy val howell9b: (Howell, Position) = {
    implicit val n: Int = 9
    implicit val tables: List[Table] = Table.tables(3, 5, 6, 8) // 2 dups
    val howell = Howell("5-table", tables, Triple(List(1), List(-2), List(-1)))
    val start = Position(List(
      Encounter(1, 1, 1, 1),
      Encounter(2, 9, 6, 2),
      Encounter(3, 8, 2, 3),
      Encounter(4, 7, 7, 4),
      Encounter(5, 6, 3, 5),
      Encounter(6, 5, 8, 6),
      Encounter(7, 4, 4, 7),
      Encounter(8, 3, 9, 8),
      Encounter(9, 2, 5, 9)))
    (howell, start)
  }


  //    showMovement(howell7)
  showMovement(howell9b)

  private def showMovement(hp: (Howell, Position)): Unit = {
    val (h, p) = hp
    println(s"\n${h.name} Howell movement")
    val rounds = for (r <- evaluateRounds(h.positions(p))) yield r
    rounds foreach println

    showGroupedEncounters("Table", groupEncounters(e => e.table))
    showGroupedEncounters("N/S", groupEncounters(e => e.n))
    showGroupedEncounters("E/W", groupEncounters(e => e.e))
    showGroupedEncounters("Board set", groupEncounters(e => e.b))
    checkEncounters(groupEncounters(e => e.b))

    // TODO sort this all out!
    def showGroupedEncounters(w: String, groups: Map[Int, List[Encounter]]): Unit = {
      println(s"Check validity grouped by $w")
      //        checkValidity("Pairs", _.pairsInOrder)
      //        checkValidity("Boards/Pairs", _.boardPairs)

      for ((k, es) <- groups.toList.sortBy(_._1)) println(s"""$w $k: ${es.mkString(", ")}""")

      //        def checkValidity(u: String, f: Encounter => List[(Int, Int)]): Unit = {
      //          val nes: List[(Int, (Int, Int))] = for ((k, es) <- groups.toList; e <- es; if e.real; z <- f(e)) yield k -> z
      //                  println(s"""$w/$u: $nes""")
      //          val distinct = nes.distinct
      //          val duplicates = nes.diff(distinct).distinct
      //          if (nes.size != distinct.size) println(s"""*** Duplicate encounters: ${nes.diff(distinct).distinct}""")
      //        }
    }

    def checkEncounters(groups: Map[Int, List[Encounter]]): Unit = {
      val triples: List[(Int, List[(Int, Int)])] = for ((k, es) <- groups.toList; e <- es; if e.real) yield k -> e.pairsInOrder.sorted
      println(s"""Triples: $triples""")
      val distinct = triples.distinct
      val duplicates = triples.diff(distinct).distinct
      if (triples.size != distinct.size) println(s"""*** Duplicate encounters: ${triples.diff(distinct).distinct}""")
    }

    def groupEncounters(f: Encounter => Int) = (for (r <- rounds; e <- r.position.encounters) yield e) groupBy f

  }

  def evaluateRounds(positions: List[Position]): List[Round] = for ((p, r) <- positions zip LazyList.from(1)) yield Round(r, p)
}

case class Encounter(table: Int, n: Int, e: Int, b: Int)(implicit tables: List[Table]) {
  def real: Boolean = !tables(table - 1).phantom

  implicit val nTables: Int = tables.length

  def move(moves: Trio, current: Position): Encounter = {
    val x = moves map { i => current.encounters(modulo(table - i) - 1) }
    Encounter.fromPrevious(table, x)
  }

  // Transforms a number n in the range 1-tables..infinity into the range 1..tables
  def modulo(n: Int): Int = (n + nTables - 1) % nTables + 1

  /**
    * Adjust for the situation where the N/S and E/W pairs of a team meet.
    * In this case, the N/S is bumped by the pair with number nTables+1
    */
  private val x = if (n == e) nTables + 1 else n

  val pairsInOrder: List[(Int, Int)] = List(if (e < x) e -> x else x -> e)
  val boardPairs: List[(Int, Int)] = List(b -> x, b -> e)

  override def toString: String = s"${tables(table - 1)}: $x-$e@#$b"
}

object Encounter {
  def fromPrevious(table: Int, et: Triple[Encounter])(implicit tables: List[Table]): Encounter = apply(table, et._1.n, et._2.e, et._3.b)
}