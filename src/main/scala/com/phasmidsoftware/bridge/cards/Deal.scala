package com.phasmidsoftware.bridge.cards

import com.phasmid.laScala.Shuffle
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
		* @param cardPlays the card play from each of the four hands.
		*/
	def play(cardPlays: Seq[CardPlay]): Deal = {
		require(cardPlays.size == 4)

		Deal(title, hands map (_.play(cardPlays)))
	}

	def cards: Int = hands map (_.cards) sum

	def promote(hand: Hand, suit: Suit, priority: Int): Deal = Deal(title, hands map (h => if (h == hand) h else h.promote(suit, priority)))

	override def toString: String = s"Deal $title\n${hands.mkString("\n")}"

	private def outputHand(name: String, hand: Hand): Output =
		(Output(s"$name:\t") :+ hand.neatOutput).insertBreak

	def output(output: Output): Output = (output :+ title).insertBreak ++ outputHand("North", n) ++ outputHand("East", e) ++ outputHand("South", s) ++ outputHand("West", w)
}

object Deal {
	def fromCards(title: String, cs: Seq[Card]): Deal = apply(title, (for ((cs, index) <- cs.grouped(13).zipWithIndex) yield Hand(index, cs)).toSeq)

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