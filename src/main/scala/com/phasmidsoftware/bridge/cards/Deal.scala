package com.phasmidsoftware.bridge.cards

import com.phasmid.laScala.Shuffle
import com.phasmidsoftware.bridge.mcts.Fitness
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

case class Deal(title: String, hands: Seq[Hand]) extends Outputable {
	def quit: Deal = Deal(title, hands map (_.quit))

	private val Seq(n, e, s, w) = hands

	def north: Hand = n

	def east: Hand = e

	def south: Hand = s

	def west: Hand = w

	/**
		* Play a trick (made up of four card plays).
		*
		* @param trick the card play from each of the four hands.
		*/
	def play(trick: Trick): Deal = Deal(title, hands map (_.play(trick)))

	def handsInSequence(leader: Int): Seq[Hand] = for (i <- leader to leader + 3) yield hands(i % 4)

	def evaluate(ts: Seq[Trick]): Double = ts.map(_.evaluate).sum

	def cards: Int = hands map (_.cards) sum

	/**
		* Promote the given Hand in the given suit, below the given priority and return a new Deal.
		*
		* NOTE: the concept of promotion used here views the four players independently.
		* CONSIDER: it probably makes sense to have another value of priority in a Holding which takes into account the fact that two opposite players are partners.
		*
		* @param hand     the Hand.
		* @param suit     the Suit.
		* @param priority the priority.
		* @return a new Deal where the suit holding in the hand has been promoted (if appropriate).
		*/
	def promote(hand: Hand, suit: Suit, priority: Int): Deal = Deal(title, hands map (h => if (h == hand) h else h.promote(suit, priority)))

	override def toString: String = s"Deal $title\n${hands.mkString("\n")}"

	private def outputHand(name: String, hand: Hand): Output = (Output(s"$name:\t") :+ hand.neatOutput).insertBreak

	def output(output: Output): Output = (output :+ title).insertBreak ++ outputHand("North", n) ++ outputHand("East", e) ++ outputHand("South", s) ++ outputHand("West", w)
}

object Deal {
	val CardsPerHand = 13

	def fromCards(title: String, cs: Seq[Card]): Deal = apply(title, (for ((cs, index) <- cs.grouped(CardsPerHand).zipWithIndex) yield Hand(index, cs)).toSeq)

	def apply(title: String, seed: Long = System.nanoTime()): Deal = {
		val newDeck: Seq[Card] = for (s <- Seq(Spades, Hearts, Diamonds, Clubs); r <- Seq(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)) yield Card(s, r)
		val shuffler = Shuffle[Card](seed)
		fromCards(title, shuffler(newDeck))
	}
}

/**
	* The play of a card.
	*
	* CONSIDER moving this to Card
	*
	* @param hand     the index of this hand in the Deal.
	* @param suit     rhe suit from which the card is to be played.
	* @param priority the priority of the sequence from which the card is to be played.
	*/
case class CardPlay(hand: Int, suit: Suit, priority: Int) extends Ordered[CardPlay] {
	/**
		* @return true if the top card of the sequence indicated is at least a ten.
		*/
	def isHonor: Boolean = Sequence.isHonor(priority)

	def compare(other: CardPlay): Int = Sequence.compare(priority, other.priority)

	def beats(other: CardPlay): Boolean = compare(other) < 0
}

/**
	* A set of 0 to 4 card plays which describe the state of the current trick.
	*
	* @param plays  the sequence of plays (in sequence).
	* @param leader the index of the leader to the suit.
	* @param suit   the suit led.
	*/
case class Trick(plays: Seq[CardPlay], leader: Int, suit: Suit) {
	/**
		* @return the index of the hand next to play in this trick.
		*/
	def next: Int = Hand.next(leader, size)

	/**
		* @return the number of cards currently in this Trick.
		*/
	def size: Int = plays.size

	/**
		* @return true if this trick is complete (size == 4).
		*/
	def isComplete: Boolean = size == 4

	/**
		* @return (optionally) the card play that was led to start the trick.
		*/
	def led: Option[CardPlay] = plays.headOption

	/**
		* @return the most recent play of this Trick.
		* @throws CardException if this Trick has no plays
		*/
	def last: CardPlay = if (plays.nonEmpty) plays.last else throw CardException(s"Trick: logic error: empty trick")

	/**
		* Add to the current Trick.
		*
		* @param play a card play which is to be added to the sequence of card plays.
		* @return a new Trick, with one more card play than this.
		*/
	def :+(play: CardPlay): Trick = Trick(plays :+ play, leader, suit)

	/**
		* @return true if the first card in this trick is an honor.
		*/
	def isHonorLed: Boolean = led match {
		case Some(p) => p.isHonor
		case None => false
	}

	def evaluate: Double = value.getOrElse(0.5)

	override def toString: String = s"T$leader: $suit ${plays.mkString("{", ",", "}")}"

	lazy val winner: Option[Int] =
		if (isComplete)
			Some(plays.maxBy(p => if (p.suit == suit) Ace.priority - p.priority else 0).hand)
		else None

	lazy val value: Option[Double] = winner map (x => if (x % 2 == 0) 1 else 0)
}

object Trick {

	implicit object TrickFitness extends Fitness[Trick] {
		override def fitness(x: Trick): Double = x.evaluate
	}

	def create(leader: Int, suit: Suit, plays: CardPlay*): Trick = apply(plays, leader, suit)

	def create(leader: Int, suit: Suit): Trick = create(leader, suit)
}