/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.pbn

import com.phasmidsoftware.bridge.cards.Deal

/**
  * This class represents the PBN (Portable Bridge Notation) for a set of games.
  *
  * @param games the games for this PBN
  */
case class PBN(games: Seq[Game]) extends Iterable[Game] with (Int => Game) {

  /**
    * Method to get the ith game.
    *
    * @param i the index of the game wanted (0 .. N-1)
    * @return the designated Game.
    */
  override def apply(i: Int): Game = games(i)

  def iterator: Iterator[Game] = games.iterator

  def sorted(implicit ev: Ordering[Game]): Seq[Game] = games.sorted
}

/**
  * This class represents a game in PBN form.
  *
  * @param tagPairs a list of tagPairs where each tagPair is a Name->DetailedValue pair.
  */
case class Game(tagPairs: Seq[(Name, DetailedValue)]) extends Iterable[(Name, DetailedValue)] with (String => DetailedValue) {
  def asMap: Map[String, DetailedValue] = tagPairs.map(mapper.tupled).toMap

  override def apply(w: String): DetailedValue = tagPairs.toMap.apply(Name(w))

  override def toString: String = s"Game: ${tagPairs.toMap}"

  def iterator: Iterator[(Name, DetailedValue)] = tagPairs.iterator

  private val mapper: (Name, DetailedValue) => (String, DetailedValue) = (n, v) => n.toString -> v
}

object Game {

  implicit object GameOrdering extends Ordering[Game] {
    def compare(x: Game, y: Game): Int = {
      import DetailedValue._
      val eventName = "Event"
      val cfEvent = DetailedValueOrdering.compare(x(eventName), y(eventName))
      if (cfEvent != 0) cfEvent
      else {
        // TODO order by date
        val boardName = "Board"
        implicitly[Ordering[Int]].compare(x(boardName).toInt, y(boardName).toInt)
      }
    }
  }

}

case class DetailedValue(value: Value, detail: Seq[String]) extends Value {
  override def toInt: Int = value.toInt

  override def toString: String = s""""$value"${detail.mkString(" ", ",", "")}"""
}

object DetailedValue {
  def trim(value: Value, detail: Seq[String]): DetailedValue = DetailedValue(value, trim(detail))

  // TODO simplify this because we no longer have newlines embedded in the strings.
  private def trim(detail: Seq[String]): Seq[String] = (detail.reverse.filter(_.nonEmpty) match {
    case Nil => Nil
    case h :: t =>
      h.reverse.replaceFirst("\n", "") match {
        case "" => t
        case x => x.reverse :: t
      }
  }).reverse

  implicit object DetailedValueOrdering extends Ordering[DetailedValue] {
    def compare(x: DetailedValue, y: DetailedValue): Int = Value.ValueOrdering.compare(x.value, y.value)
  }

}

case class Name(name: String) {
  override def toString: String = name
}

trait Value {
  def asString: String = this match {
    case StringValue(w) => w
    case _ => throw PBNException(s"value: $this is not a StringValue")
  }

  def toInt: Int = this match {
    case StringValue(w) =>
      val intR = """(-?\d+)""".r
      w match {
        case intR(x) => try x.toInt catch {
          case e: Throwable => throw PBNException(s"string: $x cannot be converted to an Int because", e)
        }
        case _ => throw PBNException(s"value: $this cannot be converted to an Int")
      }
    case _ => throw PBNException(s"value: $this is not a StringValue and thus cannot be converted to an Int")
  }
}

object Value {

  implicit object ValueOrdering extends Ordering[Value] {
    def compare(x: Value, y: Value): Int = x match {
      case sv1: StringValue =>
        y match {
          case sv2: StringValue => sv1.value.compareTo(sv2.value)
          case _ => throw PBNException(s"cannot compare StringValue $sv1 with ${y.getClass}")
        }
      case _ => throw PBNException(s"cannot compare ${x.getClass} with ${y.getClass}")
    }
  }

}

case class DateValue(year: Int, month: Int, day: Int) extends Value {
  // TODO put into yyy.mm.dd format
  //		def value: LocalDate =
  override def toString: String = ""
}

/**
  * Create a Deal from a sequence of sequence of Strings, each representing a holding.
  *
  * CONSIDER making this an implicit class
  *
  * @param q   the compass point of the first hand.
  * @param rss four hands in order NESW of four holdings in order SHDC.
  * @return a new Deal.
  */
case class DealValue(q: String, rss: Seq[Seq[String]]) extends Value {

  // CONSIDER adding a proper title to this deal.
  def deal: Deal = Deal.fromHandStrings("", q, rss)
}

case class SetValue(ws: Seq[String]) extends Value

case class StringValue(value: String) extends Value {
  override def toString: String = value
}

//noinspection ScalaStyle
case class PBNException(w: String, cause: Throwable = null) extends Exception(w, cause)
