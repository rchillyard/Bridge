package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.implicitConversions

/**
	* This class models a playing card.
	*
	* CONSIDER removing the priority parameter because priority can be encoded into a sequence instead.
	*
	* @param suit the suit of the card (spades, hearts, diamonds, or clubs).
	* @param rank the rank of the card (2 thru A).
	*/
case class Card(suit: Suit, rank: Rank) {
	def priority: Int = rank.priority

	override def toString: String = s"$suit$rank" // Bridge order (not Poker)
}

/**
	* A (non-empty) sequence of cards.
	*
	* @param priority the number of higher-ranking cards in the suit
	* @param cards    the cards
	*/
case class Sequence(priority: Int, cards: Seq[Card]) {

	require(cards.nonEmpty)

	/**
		* Method to truncate a Sequence (by playing a card: deemed to be the lowest card)
		*
		* @return an optional Sequence: Some(s) if s is a valid sequence, otherwise None if the sequence has been eliminated.
		*/
	def truncate: Option[Sequence] = {
		val remainder = cards.init
		if (remainder.nonEmpty) Some(Sequence(priority, remainder)) else None
	}

	def length: Int = cards.size

	def head: Card = cards.head

	def promote: Sequence = if (canPromote) Sequence(priority - 1, cards) else throw CardException(s"cannot promote priority $this")

	/**
		* Merge this Sequence into a sequence of Sequences (ss).
		* If ss is empty or its last element cannot be combined with this, then we simply add this to ss.
		* Otherwise, we take the other elements of ss and add a combined Sequence from the last element and this.
		*
		* @param ss a sequence of Sequences.
		* @return a new sequence of Sequences.
		*/
	def merge(ss: Seq[Sequence]): Seq[Sequence] = if (ss.nonEmpty && ss.last.canCombine(this)) ss.init :+ (ss.last ++ this) else ss :+ this

	def ++(s: Sequence): Sequence = if (canCombine(s)) Sequence(priority, cards ++ s.cards) else throw CardException(s"cannot combine Sequences: $this and $s")

	override def toString: String = s"${cards.map(_.rank).mkString("")}[$priority]"

	private def canPromote = priority > 0

	private def canCombine(s: Sequence) = priority + length == s.priority
}

object Sequence {
	/**
		* Method to create a Sequence from a non-empty list of Cards.
		*
		* @param cs the list of Cards (must be non-empty).
		* @return a new Sequence.
		*/
	def apply(cs: Seq[Card]): Sequence = apply(cs.head.priority, cs)

	implicit object SequenceOrdering extends Ordering[Sequence] {
		override def compare(x: Sequence, y: Sequence): Int = x.priority - y.priority
	}
}
/**
	* This class models a holding in a suit.
	*
	* @param sequences the sequences (expected to be in order of rank).
	* @param suit      the suit of this holding.
	*/
case class Holding(sequences: Seq[Sequence], suit: Suit) extends Outputable {

	require(isVoid || maybeSuit.get == suit)

	def size: Int = sequences.size

	def cards: Seq[Card] = for (s <- sequences; c <- s.cards) yield c

	def isVoid: Boolean = sequences.isEmpty

	def promote(priority: Int): Holding = {
		def maybePromote(sequence: Sequence): Sequence = (sequence.priority - priority).signum match {
			case -1 => sequence
			case 1 => sequence.promote
			case 0 => throw CardException(s"Holding: logic error: $this cannot self-promote for priority $priority")
		}

		Holding((sequences map maybePromote).foldLeft[Seq[Sequence]](Nil)((ss, s) => s.merge(ss)), suit)
	}

	/**
		* Method to remove (i.e. play) a card from this Holding.
		*
		* @param priority the sequence from which the card will be played.
		* @return a new Holding with the sequence either truncated or eliminated entirely.
		*/
	def -(priority: Int): Holding = {
		val sos: Seq[Option[Sequence]] = for (s <- sequences) yield if (s.priority == priority) s.truncate else Some(s)
		Holding(sos.flatten, suit)
	}

	override def toString: String = s"{$suit: ${sequences.mkString(", ")}}"

	/**
		* NOTE: this is used temporarily because Output is messing up
		*
		* @return
		*/
	def neatOutput: String = s"$suit${Holding.ranksToString(cards map (_.rank))}"

	def output(o: Output): Output = o :+ suit.toString :+ Holding.ranksToString(cards map (_.rank))

	private def maybeSuit: Option[Suit] = cards.headOption map (_.suit)
}

/**
	* Companion object for Holding.
	*/
object Holding {

	def apply(suit: Suit, ranks: Rank*): Holding = {
		val cards = ranks map (rank => Card(suit, rank))
		val cXsXm = (for ((c, i) <- cards.zipWithIndex) yield i - c.priority -> c).groupBy(_._1)
		val ss = cXsXm.values map (cXs => Sequence(cXs.map(_._2)))
		apply(ss.toSeq.sorted, suit)
	}

	implicit def parseHolding(s: String): Holding = create(Card.parser.parseRanks(s.tail), Suit(s.head))

	implicit object RankOrdering extends Ordering[Rank] {
		override def compare(x: Rank, y: Rank): Int = -x.priority + y.priority
	}

	def create(ranks: Seq[Rank], suit: Suit) = Holding(suit, ranks.sorted.reverse: _*)

	def ranksToString(ranks: Seq[Rank]): String = if (ranks.nonEmpty) ranks.mkString("", "", "") else "-"
}

/**
	* This class models a bridge hand (four suits).
	*
	* @param index    the index of this hand within a Deal.
	* @param holdings the four holdings (as a Map).
	*/
case class Hand(index: Int, holdings: Map[Suit, Holding]) extends Outputable {

	/**
		* Apply a sequence of CardPlay operations to this Hand.
		* Note that we have to sort the order of the card plays so that we aren't trying to promote sequences where a card has already been played.
		*
		* TODO: however, we need to realize that in actual play, the CardPlay objects happen one at a time and so cannot be ordered.
		* This suggests that we need to delay the application of promotion until after a trick is quitted.
		*
		* @param cardPlays the list of card plays
		* @return a new Hand based on this Hand and all of the card plays.
		*/
	def play(cardPlays: Seq[CardPlay]): Hand = cardPlays.sortWith(_.priority > _.priority).foldLeft[Hand](this)(_ play _)

	/**
		* Create new Hand based on the play of a card.
		*
		* @param cardPlay the card play.
		* @return a new Hand
		*/
	def play(cardPlay: CardPlay): Hand = {
		val priority = cardPlay.priority
		if (cardPlay.hand == index)
			this - (cardPlay.suit, priority)
		else
			promote(cardPlay.suit, priority)
	}

	/**
		* Method to get the count of the cards in this Hand.
		*
		* @return the sum of the cards in the holdings
		*/
	def cards: Int = holdings.values.map(_.cards.size).sum

	/**
		* Method to eliminate (play) a card from the given suit and sequence.
		*
		* @param suit     the suit.
		* @param priority the priority of the sequence from which we take a card.
		* @return a new Hand, with one less card.
		*/
	def -(suit: Suit, priority: Int): Hand =
		Hand(index, holdings + (suit -> (holdings(suit) - priority)))

	def promote(suit: Suit, priority: Int): Hand =
		Hand(index, holdings + (suit -> holdings(suit).promote(priority)))

	override def toString: String = {
		// TODO figure out why we can't just import SuitOrdering from Suit
		implicit object SuitOrdering extends Ordering[Suit] {
			override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
		}
		val keys = holdings.keys.toSeq.sorted.reverse
		s"""${(for (k <- keys) yield s"${holdings(k)}").mkString("", "\n", "")}"""
	}

	/**
		* Temporary while Output is broken
		*
		* @return
		*/
	def neatOutput: String = {
		// TODO figure out why we can't just import SuitOrdering from Suit
		implicit object SuitOrdering extends Ordering[Suit] {
			override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
		}
		val keys = holdings.keys.toSeq.sorted.reverse
		(for (k <- keys) yield holdings(k).neatOutput).mkString(" ")
	}

	def output(output: Output): Output = {
		// TODO figure out why we can't just import SuitOrdering from Suit
		implicit object SuitOrdering extends Ordering[Suit] {
			override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
		}
		val keys = holdings.keys.toSeq.sorted.reverse
		output ++ (for (k <- keys) yield holdings(k).output(Output.empty))
	}
}


/**
	* Trait defining the properties of a suit
	*/
trait Suit {
	val isRound: Boolean
	val isRed: Boolean
}

/**
	* Trait defining the properties of a rank
	*/
trait Rank extends Priority {
	val isHonor: Boolean
}


/**
	* Trait defining the priority: the number of objects which precede this object in the ordering.
	*/
trait Priority {
	def priority: Int
}

/**
	* Abstract base class for Suit.
	*
	* @param isRound true if this is hearts or clubs.
	* @param isRed   true if this is hearts or diamonds.
	*/
abstract class BaseSuit(val isRound: Boolean, val isRed: Boolean) extends Suit with Priority {
	def priority: Int = Card.bool2Int(isRound) + 2 * Card.bool2Int(isRound ^ isRed)

	override def toString: String = List("S", "H", "D", "C")(priority)
}

case object Spades extends BaseSuit(false, false)

case object Hearts extends BaseSuit(true, true)

case object Diamonds extends BaseSuit(false, true)

case object Clubs extends BaseSuit(true, false)

/**
	* Companion object for Suit.
	*/
object Suit {
	/**
		* Implicitly convert String to Suit.
		*
		* @param s the String.
		* @return the Rank.
		*/
	implicit def convertStringToSuit(s: String): Suit = apply(s.head)

	/**
		* Explicitly convert a Char into a Suit.
		*
		* @param c the Char.
		* @return the Suit.
		*/
	def apply(c: Char): Suit = c match {
		case 'S' | 's' => Spades
		case 'H' | 'h' => Hearts
		case 'D' | 'd' => Diamonds
		case 'C' | 'c' => Clubs
		case _ => throw CardException(s"$c is not a suit")
	}

	/**
		* Define an ordering for Suits.
		*/
	implicit object SuitOrdering extends Ordering[Suit] {
		override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
	}

}

/**
	* Abstract base class for Rank.
	*
	* @param priority the priority of this Rank.
	* @param isHonor  true if this Rank is an honor.
	*/
abstract class BaseRank(val priority: Int, val isHonor: Boolean) extends Rank with Priority {

	override def toString: String = if (isHonor) List("A", "K", "Q", "J", "T")(priority) else (14 - priority).toString

	private def canEqual(other: Any): Boolean = other.isInstanceOf[BaseRank]

	override def equals(other: Any): Boolean = other match {
		case that: BaseRank =>
			(that canEqual this) &&
				priority == that.priority
		case _ => false
	}

	override def hashCode(): Int = {
		val state = Seq(priority)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}
}

/**
	* Companion object for Rank
	*/
object Rank {

	/**
		* Implicit converter from String to Rank
		*
		* @param s the String.
		* @return the Rank.
		*/
	implicit def parseRank(s: String): Rank = apply(s)

	/**
		* Defines an ordering of Ranks
		*/
	implicit object RankOrdering extends Ordering[Rank] {
		override def compare(x: Rank, y: Rank): Int = -x.priority + y.priority
	}

	/**
		* Method to create a Rank from an honor String (A, K, Q, J, or T)
		*
		* @param w the String
		* @return the Rank.
		*/
	def honor(w: String): Rank = w match {
		case "A" => Ace
		case "K" => King
		case "Q" => Queen
		case "J" => Jack
		case "T" => Ten
		case _ => throw CardException(s"$w is not an honor rank")
	}

	/**
		* Apply method for rank r.
		* NOTE: this also works for honor ranks.
		*
		* @param r the number of spots on the card.
		* @return the Rank.
		*/
	def apply(r: Int): Rank = Spot(r)

	/**
		* Method to (explicitly) create a Rank from a String
		*
		* @param s the String.
		* @return the Rank.
		*/
	def apply(s: String): Rank = s match {
		case spotR(n) => Spot(n.toInt)
		case honorR(w) => honor(w)
		case _ => throw CardException(s"$s is not a rank")
	}

	private val spotR = """(\d\d?)""".r
	private val honorR = """([AKQJT])""".r
}

/**
	* Case class corresponding to a spot Rank.
	*
	* @param spot the spot value.
	*/
case class Spot(spot: Int) extends BaseRank(14 - spot, spot > 9)

case object Deuce extends BaseRank(12, false)

case object Trey extends BaseRank(11, false)

case object Four extends BaseRank(10, false)

case object Five extends BaseRank(9, false)

case object Six extends BaseRank(8, false)

case object Seven extends BaseRank(7, false)

case object Eight extends BaseRank(6, false)

case object Nine extends BaseRank(5, false)

case object Ten extends BaseRank(4, true)

case object Jack extends BaseRank(3, true)

case object Queen extends BaseRank(2, true)

case object King extends BaseRank(1, true)

case object Ace extends BaseRank(0, true)

object Hand {
	def apply(index: Int, cs: Seq[Card]): Hand = Hand(index, for ((suit, cards) <- cs.groupBy(c => c.suit)) yield (suit, Holding(suit, (cards map (_.rank)).sorted.reverse: _*)))

	def from(index: Int, ws: String*): Hand = {
		val tuples = for (w <- ws; h = Holding.parseHolding(w)) yield h.suit -> h
		Hand(index, tuples.toMap)
	}
}

/**
	* Companion object for Card.
	*/
object Card {

	/**
		* Explicit conversion of String to Card
		*
		* @param s the String.
		* @return the Card.
		*/
	def apply(s: String): Card = {
		val suit = s.head match {
			case 'S' | 's' => Spades
			case 'H' | 'h' => Hearts
			case 'D' | 'd' => Diamonds
			case 'C' | 'c' => Clubs
			case c => throw CardException(s"$c is not a suit symbol")
		}
		val rank = Rank(s.tail)
		Card(suit, rank)
	}

	/**
		* Implicit conversion of String to Card.
		*
		* @param s the String.
		* @return the Card.
		*/
	implicit def convertStringToCard(s: String): Card = apply(s)

	/**
		* Defines an ordering of Ranks
		*/
	implicit object CardOrdering extends Ordering[Card] {
		override def compare(x: Card, y: Card): Int = {
			import Rank._
			import Suit._
			val cf = SuitOrdering.compare(x.suit, y.suit)
			if (cf != 0) cf
			else RankOrdering.compare(x.rank, y.rank)
		}
	}

	private[cards] def bool2Int(b: Boolean): Int = if (b) 1 else 0

	private[cards] val parser = new RankParser
}

/**
	* Exception defined for this module.
	*
	* @param w the message.
	*/
case class CardException(w: String) extends Exception(w)

import scala.util.parsing.combinator.JavaTokenParsers

/**
	* Parser of rank strings.
	*/
class RankParser extends JavaTokenParsers {

	/**
		* Method to parse ranks as a sequence of Rank
		*
		* @param w the String to parse
		* @return a Seq[Rank]
		*/
	def parseRanks(w: String): Seq[Rank] = {
		val parsed: ParseResult[Seq[Rank]] = parseAll(holding, w)
		parsed match {
			case Success(rs, _) => rs
			case Failure(x, input) => throw CardException(x + ": " + input)
			case Error(_, _) => throw CardException("error")
		}
	}

	def holding: Parser[List[Rank]] = rep(rank) ^^ (_ map Rank.apply)

	def rank: Parser[String] = """[2-9]""".r | """[AKQJT]""".r | "10" | failure("invalid rank")
}

object Log {
	def log[X](message: String)(x: => X): X = {
		lazy val xx = x
		System.err.println(s"log: $message: $xx")
		xx
	}
}