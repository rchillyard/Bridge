package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.implicitConversions

/**
	* A (non-empty) sequence of cards.
	*
	* @param priority the number of higher-ranking cards in the suit
	* @param cards    the cards
	*/
case class Sequence(priority: Int, cards: Seq[Card]) extends Evaluatable {

	require(cards.nonEmpty)

	/**
		* @return true if the top card of the sequence indicated is at least a ten.
		*/
	lazy val isHonor: Boolean = Sequence.isHonor(priority)

	/**
		* @return the length of this sequence times one-half to the power of priority.
		*/
	def evaluate: Double = _evaluate

	/**
		* Method to truncate a Sequence (by playing a card: deemed to be the lowest card)
		*
		* @return an optional Sequence: Some(s) if s is a valid sequence, otherwise None if the sequence has been eliminated.
		*/
	lazy val truncate: Option[Sequence] = {
		val remainder = cards.init
		if (remainder.nonEmpty) Some(Sequence(priority, remainder)) else None
	}

	/**
		* @return The number of cards in this Sequence.
		*/
	lazy val length: Int = cards.size

	/**
		* @return the highest card of the sequence.
		*/
	lazy val head: Card = cards.head

	/**
		* Method to promote this sequence by one.
		*
		* @return a new Sequence with the same cards but with a lower priority.
		*/
	lazy val promote: Sequence = if (canPromote) Sequence(priority - 1, cards) else throw CardException(s"cannot promote priority $this")

	/**
		* Merge this Sequence into a sequence of Sequences (ss).
		* If ss is empty or its last element cannot be combined with this, then we simply add this to ss.
		* Otherwise, we take the other elements of ss and add a combined Sequence from the last element and this.
		*
		* @param ss a sequence of Sequences.
		* @return a new sequence of Sequences.
		*/
	def merge(ss: Seq[Sequence]): Seq[Sequence] = if (ss.nonEmpty && ss.last.canCombine(this)) ss.init :+ (ss.last ++ this) else ss :+ this

	/**
		* Not currently used?
		*
		* @param s the input Sequence
		* @return the concatenation of this and s.
		*/
	def ++(s: Sequence): Sequence = if (canCombine(s)) Sequence(priority, cards ++ s.cards) else throw CardException(s"cannot combine Sequences: $this and $s")

	override def toString: String = s"${cards.map(_.rank).mkString("")}[$priority]"

	private lazy val canPromote = priority > 0

	private def canCombine(s: Sequence) = priority + length == s.priority

	private lazy val _evaluate = cards.length * math.pow(0.5, priority)
}

object Sequence {
	/**
		* Method to create a Sequence from a non-empty list of Cards.
		*
		* @param cs the list of Cards (must be non-empty).
		* @return a new Sequence.
		*/
	def apply(cs: Seq[Card]): Sequence = apply(cs.head.priority, cs)

	/**
		* @return true if the top card of the sequence with the given priority is at least a ten.
		*/
	def isHonor(priority: Int): Boolean = priority <= 4

	/**
		* Method to compare two priorities.
		*
		* @param p1 the first priority.
		* @param p2 the second priority.
		* @return -1, 0, or 1 according to whether p1 is less than, equal to, or greater than p2.
		*/
	def compare(p1: Int, p2: Int): Int = p1.compareTo(p2)

	/**
		* Method to determine if the priority of p1 is "higher" (less) than the priority of p2.
		*
		* @param p1 the first priority.
		* @param p2 the second priority.
		* @return true if p1 out ranks p2.
		*/
	def higher(p1: Int, p2: Int): Boolean = compare(p1, p2) < 0

	implicit object SequenceOrdering extends Ordering[Sequence] {
		override def compare(x: Sequence, y: Sequence): Int = x.priority - y.priority
	}

}

trait Quittable[X] {
	/**
		* Method to enact the pending promotions on this Holding.
		*
		* @return an eagerly promoted X.
		*/
	def quit: X
}

/**
	* This class models a holding in a suit.
	*
	* @param sequences  the sequences (expected to be in order of rank).
	* @param suit       the suit of this holding.
	* @param promotions a list of promotions that should be applied on quitting a trick.
	*/
case class Holding(sequences: Seq[Sequence], suit: Suit, promotions: Seq[Int] = Nil) extends Outputable[Unit] with Quittable[Holding] with Evaluatable {

	require(isVoid || maybeSuit.get == suit)

	/**
		* @return the number of sequences in this Holding
		*/
	lazy val size: Int = sequences.size

	/**
		* @return the number of cards in this Holing (i.e. the suit length)
		*/
	lazy val length: Int = sequences.map(_.length).sum

	/**
		* Optionally yield a Sequence that matches the given priority.
		*
		* @param priority the priority to be matched.
		* @return an Option[Sequence].
		*/
	def sequence(priority: Int): Option[Sequence] = sequences.find(s => s.priority == priority)

	/**
		* @return the all of the cards in this Holding.
		*/
	lazy val cards: Seq[Card] = for (s <- sequences; c <- s.cards) yield c

	/**
		* NOTE: this is only very approximately correct and is used as a heuristic.
		* In particular, a suit such as AKJT should evaluate as somewhere around 3.5.
		*
		* @return a sum of the evaluations of each sequence.
		*/
	def evaluate: Double = _evaluate

	/**
		* Method to choose plays according to the prior plays and the cards in this Holding.
		*
		* @param deal  the deal to which these plays will belong.
		* @param hand  the index of the Hand containing this Holding.
		* @param trick the current state of this trick (i.e. the prior plays).
		* @return a sequence of all possible plays, starting with the ones most suited to the appropriate strategy.
		*/
	def choosePlays(deal: Deal, hand: Int, trick: Trick): Seq[CardPlay] = if (suit == trick.suit) {
		val strategy: Strategy =
			trick.size match {
				case 0 => if (hasHonorSequence) LeadHigh else FourthBest
				case 1 => if (trick.isHonorLed || realSequences.nonEmpty) Cover else Duck
				case 2 => Finesse
				case 3 => WinIt
				case x => throw CardException(s"too many prior plays: $x")
			}
		choosePlays(deal, hand, strategy)
	}
	else throw CardException(s"Holding.choosePlays logic error: trick suit ${trick.suit} does not match this suit $suit")

	/**
		* For now, we ignore strategy which is only used to ensure that we try the likely more successful card play first.
		*
		* @param deal     the deal to which these plays will belong.
		* @param hand     the index of this Hand (N, E, S, W).
		* @param strategy the recommended strategy (currently ignored).
		* @return a sequence of CardPlay objects.
		*/
	def choosePlays(deal: Deal, hand: Int, strategy: Strategy): Seq[CardPlay] = for (s <- sequences) yield CardPlay(deal, hand, suit, s.priority)

	/**
		* @return true if this Holding is void.
		*/
	lazy val isVoid: Boolean = sequences.isEmpty

	/**
		* Method to promote this holding if it ranks lower than the given priority.
		* NOTE: this does not immediately change the priority of any sequences in this Holding--
		* instead we use a lazy approach--adding to the list of pending promotions which will be enacted when quit is called.
		*
		* @param priority the priority.
		* @return a new Holding with the promotion added to the pending list.
		*/
	def promote(priority: Int): Holding = Holding(sequences, suit, promotions :+ priority)

	/**
		* Method to enact the pending promotions on this Holding.
		*
		* @return a newly promoted Holding.
		*/
	def quit: Holding = _quit

	/**
		* TODO implement me.
		*
		* Determine the fourth best card.
		*
		* @return the fourth best card from this Holding.
		*/
	lazy val fourthBest: CardPlay = ???

	/**
		* Method to remove (i.e. play) a card from this Holding.
		*
		* @param priority the sequence from which the card will be played.
		* @return a new Holding with the sequence either truncated or eliminated entirely.
		*/
	def -(priority: Int): Holding = {
		val sos: Seq[Option[Sequence]] = for (s <- sequences) yield if (s.priority == priority) s.truncate else Some(s)
		Holding(sos.flatten, suit, promotions)
	}

	override def toString: String = s"{$suit: ${sequences.mkString(", ")}} " + (if (promotions.nonEmpty) promotions.mkString(", ") else "(clean)")

	/**
		* NOTE: this is used temporarily because Output is messing up
		*
		* @return
		*/
	lazy val neatOutput: String = s"$suit${Holding.ranksToString(cards map (_.rank))}"

	def output(output: Output, xo: Option[Unit] = None): Output = output :+ suit.toString :+ Holding.ranksToString(cards map (_.rank))

	private lazy val _quit = {

		def applyPromotions(sequence: Sequence): Sequence = {
			val promotion = promotions.count(_ < sequence.priority)
			Sequence(sequence.priority - promotion, sequence.cards)
		}

		val ss: Seq[Sequence] = sequences map applyPromotions
		Holding(ss.foldLeft[Seq[Sequence]](Nil)((r, s) => s.merge(r)), suit, Nil)
	}

	private lazy val hasHonorSequence: Boolean = realSequences exists (_.priority < 4)

	private lazy val realSequences = sequences filter (_.cards.lengthCompare(1) > 0)

	private def canWin(priorPlays: Seq[CardPlay]): Boolean = if (priorPlays.nonEmpty && !isVoid) priorPlays.min.priority > sequences.head.priority else true

	private lazy val maybeSuit: Option[Suit] = cards.headOption map (_.suit)

	private lazy val _evaluate: Double = {
		// TODO for now, I'm going to use iteration and var !!
		var result = 0.0
		var cards = 0
		for (i <- sequences.indices) {
			val sequence = sequences(i)
			val x = sequence.evaluate
			result += x * math.pow(2, cards)
			cards += sequence.length
		}
		result
	}
}

/**
	* Companion object for Holding.
	*/
object Holding {

	def apply(suit: Suit, ranks: Rank*): Holding = {
		val cards = ranks map (rank => Card(suit, rank))
		val cXsXm = (for ((c, i) <- cards.zipWithIndex) yield i - c.priority -> c).groupBy(_._1)
		val ss = cXsXm.values map (cXs => Sequence(cXs.map(_._2)))
		apply(ss.toSeq.sorted, suit, Nil)
	}

	implicit def parseHolding(s: String): Holding = create(Card.parser.parseRanks(s.tail), Suit(s.head))

	implicit object RankOrdering extends Ordering[Rank] {
		override def compare(x: Rank, y: Rank): Int = -x.priority + y.priority
	}

	// TODO merge the two create methods
	def create(suit: Suit, cards: Seq[Card]): Holding = apply(suit, (cards map (_.rank)).sorted.reverse: _*)

	def create(ranks: Seq[Rank], suit: Suit) = Holding(suit, ranks.sorted.reverse: _*)

	def ranksToString(ranks: Seq[Rank]): String = if (ranks.nonEmpty) ranks.mkString("", "", "") else "-"
}

/**
	* This class models a bridge hand (four suits).
	*
	* @param deal     the deal to which this Hand belongs.
	* @param index    the index of this hand within the deal.
	* @param holdings the four holdings (as a Map).
	*/
case class Hand(deal: Deal, index: Int, holdings: Map[Suit, Holding]) extends Outputable[Unit] with Quittable[Hand] with Playable[Hand] with Evaluatable {

	/**
		* @return the index of the next hand in sequence around the table.
		*/
	lazy val next: Int = Hand.next(index)

	/**
		* CONSIDER: using strength of suit as a decider between equal lengths.
		*
		* @return the longest suit as a Holding. If there are two such suits, it is arbitrary which one will be chosen.
		*/
	def longestSuit: Holding = _longestSuit

	/**
		* @return the sum of the evaluations for each suit.
		*/
	def evaluate: Double = _evaluate

	/**
		* Apply a sequence of CardPlay operations to this Hand.
		*
		* @param trick the trick.
		* @return a new Hand based on this Hand and all of the card plays.
		*/
	def playAll(trick: Trick): Hand = trick.plays.foldLeft[Hand](this)(_ play _)

	/**
		* Create new Hand based on the play of a card.
		*
		* @param cardPlay the card play.
		* @return a new Hand.
		*/
	def play(cardPlay: CardPlay): Hand = {
		val priority = cardPlay.priority
		if (cardPlay.hand == index)
			this - (cardPlay.suit, priority)
		else
			promote(cardPlay.suit, priority)
	}

	/**
		* Method to determine possible discard plays.
		*
		* @param trick the current state of the Trick.
		* @return a sequence of card plays.
		*/
	def discard(trick: Trick): Seq[CardPlay] = {
		for {
			// first get the holdings from the other suits in order of length
			h <- holdings.flatMap { case (k, v) => if (k != trick.suit) Some(v) else None }.toSeq.sortWith(_.length < _.length)
			ps <- h.choosePlays(deal, index, Duck)
		} yield ps
	}

	/**
		* Choose the plays for this Hand, based on the prior plays.
		*
		* @param trick the prior plays to the current trick.
		* @return a Seq[CardPlay].
		*/
	def choosePlays(trick: Trick): Seq[CardPlay] = {
		val holding = holdings(trick.suit)
		if (holding.isVoid) discard(trick)
		else holding.choosePlays(deal, index, trick)
	}

	/**
		* Method to get the count of the cards in this Hand.
		*
		* @return the sum of the cards in the holdings
		*/
	lazy val cards: Int = holdings.values.map(_.cards.size).sum

	/**
		* Method to eliminate (play) a card from the given suit and sequence.
		*
		* @param suit     the suit.
		* @param priority the priority of the sequence from which we take a card.
		* @return a new Hand, with one less card.
		*/
	def -(suit: Suit, priority: Int): Hand =
		Hand(deal, index, holdings + (suit -> (holdings(suit) - priority)))

	def promote(suit: Suit, priority: Int): Hand =
		Hand(deal, index, holdings + (suit -> holdings(suit).promote(priority)))

	/**
		*
		* @return an eagerly promoted Hand.
		*/
	def quit: Hand = _quit

	/**
		* Method to output this object (and, recursively, all of its children).
		*
		* @param output the output to append to.
		* @param xo     an optional value of X, defaulting to None.
		* @return a new instance of Output.
		*/
	def output(output: Output, xo: Option[Unit]): Output = {
		// TODO figure out why we can't just import SuitOrdering from Suit
		implicit object SuitOrdering extends Ordering[Suit] {
			override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
		}
		val keys = holdings.keys.toSeq.sorted.reverse
		output ++ (for (k <- keys) yield holdings(k).output(output.copy))
	}

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
	lazy val neatOutput: String = {
		// TODO figure out why we can't just import SuitOrdering from Suit
		implicit object SuitOrdering extends Ordering[Suit] {
			override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
		}
		val keys = holdings.keys.toSeq.sorted.reverse
		(for (k <- keys) yield holdings(k).neatOutput).mkString(" ")
	}

	private lazy val _evaluate = holdings.values.map(_.evaluate).sum

	// NOTE: only used for testing
	private lazy val _quit = Hand(deal, index, for ((k, v) <- holdings) yield k -> v.quit)

	private lazy val _longestSuit = holdings.values.maxBy(_.length)

}

/**
	* Trait defining the priority: the number of objects which precede this object in the ordering.
	*/
trait Priority {
	/**
		* The priority of this object.
		* For Rank, Ace: 0, King: 1, Deuce: 2.
		* TODO check the following:
		* For Suit, Spades: 0, Clubs: 3.
		*
		* @return
		*/
	def priority: Int
}

object Hand {

	def apply(deal: Deal, index: Int, cs: Seq[Card]): Hand = Hand(deal, index, createHoldings(cs))

	def createHoldings(cs: Seq[Card]): Map[Suit, Holding] = for ((suit, cards) <- cs.groupBy(c => c.suit)) yield (suit, Holding.create(suit, cards))

	def from(deal: Deal, index: Int, ws: String*): Hand = {
		val tuples = for (w <- ws; h = Holding.parseHolding(w)) yield h.suit -> h
		Hand(deal, index, tuples.toMap)
	}

	def next(index: Int): Int = next(index, 1)

	def next(index: Int, step: Int): Int = (index + step) % 4
}

trait Strategy {
	/**
		* @return true if we always play highest card if it will beat the existing cards.
		*/
	val winIfPossible: Boolean

	/**
		* @return true if we want to play from a sequence that is higher than any existing card.
		*/
	val split: Boolean

	/**
		* @return true if we want to play an intermediate card in the hope of winning.
		*/
	val finesse: Boolean
}

abstract class BaseStrategy(val winIfPossible: Boolean, val split: Boolean, val finesse: Boolean) extends Strategy

case object FourthBest extends BaseStrategy(false, false, false)

case object LeadHigh extends BaseStrategy(true, true, false)

case object Cover extends BaseStrategy(false, true, true)

case object WinIt extends BaseStrategy(true, false, false)

case object Duck extends BaseStrategy(false, false, false)

case object Finesse extends BaseStrategy(false, false, true)

trait Playable[X] {
	/**
		* Play a card from this Playable object.
		*
		* @param cardPlay the card play.
		* @return a new Playable.
		*/
	def play(cardPlay: CardPlay): X
}

trait Evaluatable {

	/**
		* Evaluate this Evaluatable object for its (heuristic) trick-taking capability.
		*
		* @return a Double
		*/
	def evaluate: Double
}
