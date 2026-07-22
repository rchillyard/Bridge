/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.pbn

import com.phasmidsoftware.bridge.cards.*
import com.phasmidsoftware.gambit.util.LazyLogger

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
  private val logger = LazyLogger(getClass)

  def asMap: Map[String, DetailedValue] = tagPairs.map(mapper.tupled).toMap

  def title: String =
    val site = apply("Site").value.asString
    val event = apply("Event").value.asString
    val board = apply("Board").value.asString
    val dateValue: Value = apply("Date").value
    val date = dateValue.toString
    s"$site: $event ($date), board: $board"

  override def apply(w: String): DetailedValue = tagPairs.toMap.apply(Name(w))

  override def toString: String = s"gameChecker: ${tagPairs.toMap}"

  def iterator: Iterator[(Name, DetailedValue)] = tagPairs.iterator

  /**
    * Parses this game's `OptimumResultTable` tag (one line per declarer/strain/trick-count
    * entry, e.g. "N NT 8") into `Contract` values.
    *
    * @throws CardException when a detail line can't be parsed as a contract.
    */
  def makableContracts: Seq[Contract] =
    val declarerTricksR = """([NESW])\s*(NT|S|H|D|C)\s*(\d+)""".r
    apply("OptimumResultTable").detail map {
      case contract@declarerTricksR(l, z, n) =>
        val declarer = "NESW".indexOf(l)
        val leader = Hand.next(declarer)
        val strain = z match {
          case "NT" => None
          case x if x.nonEmpty =>
            Some(Suit.apply(x.head))
          case _ =>
            throw CardException(s"cannot parse the contract detail: $contract")
        }
        val tricks = n.toInt
        Contract(leader, strain, tricks, declarer)
    }
    
  /**
    * Analyzes the makable contracts for the given bridge deal. This method evaluates the possible contracts
    * that can be made based on the deal's data, including the declarer, board, strain, and number of tricks.
    *
    * @param max an optional integer that sets a limit on the number of contracts analyzed. Default is 0, indicating no limit.
    * @return a sequence of optional booleans where each element indicates whether a particular contract is makable.
    *         Returns `None` if the result for the contract cannot be determined.
    *
    * @throws PBNException  if the deal provided in the context is invalid or does not meet the expected constraints.
    * @throws CardException when there is an error parsing the contract details from the provided data.
    */
  def analyzeMakableContracts(max: Int = 0): Seq[DDResult] =
    analyzeMakableContractsForGame(max, deal, makableContracts)

  /**
    * @throws PBNException if the deal is invalid (see `Deal.validate`).
    */
  def deal: Deal =
    val protoDeal = apply("Deal").value.asInstanceOf[DealValue].deal
    val d = protoDeal.copy(title = title)
    if d.validate then d else throw PBNException(s"deal is invalid: $d")

  /**
    * Analyze the makable contracts for this Game.
    *
    * @param max the maximum number of contracts to analyze (0 indicates all)
    * @param deal a Deal
    * @param contracts a sequence of Contract.
    * @return
    */
  def analyzeMakableContractsForGame(max: Int, deal: Deal, contracts: Seq[Contract]): Seq[DDResult] =
    val result = deal.analyzeContracts(board, contracts, max)
    result.foreach {
      case DDResult.Exact(makes, t) => logger.info(s"  => Exact($t tricks): makes=$makes")
      case DDResult.Partial(makes, t) => logger.info(s"  => Partial($t tricks, node limit): makes=$makes (qualified)")
      case DDResult.Inconclusive => logger.info(s"  => Inconclusive (node limit, no iteration completed)")
    }
    result

  def board: Int = apply("Board").toInt

  private val mapper: (Name, DetailedValue) => (String, DetailedValue) = (n, v) => n.toString -> v
}

object Game {

  implicit object GameOrdering extends Ordering[Game] {
    def compare(x: Game, y: Game): Int = {
      import DetailedValue.*
      val eventName = "Event"
      val cfEvent = DetailedValueOrdering.compare(x(eventName), y(eventName))
      if (cfEvent != 0) cfEvent
      else {
        // CONSIDER order by date
        val boardName = "Board"
        implicitly[Ordering[Int]].compare(x(boardName).toInt, y(boardName).toInt)
      }
    }
  }

}

case class DetailedValue(value: Value, detail: Seq[String]) extends Value {
  override def toInt: Int = value.toInt

  override def toString: String =
    s""""$value"${detail.mkString(" ", ",", "")}"""
}

object DetailedValue {
  def trim(value: Value, detail: Seq[String]): DetailedValue = DetailedValue(value, trim(detail))

  // CONSIDER simplify this because we no longer have newlines embedded in the strings.
  private def trim(detail: Seq[String]): Seq[String] =
    (detail.reverse.filter(_.nonEmpty) match {
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
  // CONSIDER put into yyy.mm.dd format
  //		def value: LocalDate =
  override def toString: String = s"$year/$month/$day"
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