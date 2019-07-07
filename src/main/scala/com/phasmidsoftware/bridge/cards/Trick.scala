/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{Loggable, Loggables, Output, Outputable}

import scala.language.postfixOps

/**
	* A set of 0 to 4 card plays which describe the state of the current trick.
	*
	* NOTE: we extend Outputable[Deal] because that gives the type of the second (optional) parameter to the output method.
	*
	* @param index the position of this trick in sequence, starting with one.
	* @param plays the sequence of plays (in sequence).
	*/
case class Trick(index: Int, plays: Seq[CardPlay]) extends Outputable[Deal] with Evaluatable {

	/**
		* True if plays is non empty.
		*/
	val started: Boolean = plays.nonEmpty

	lazy val suit: Option[Suit] = plays.headOption.map(_.suit)

	lazy val leader: Option[Int] = plays.headOption.map(_.hand)

	/**
		* @return the index of the hand next to play in this trick.
		*/
	lazy val next: Option[Int] = leader.map(Hand.next(_, size))

	/**
		* @return the number of cards currently in this Trick.
		*/
	lazy val size: Int = plays.size

	/**
		* @return true if this trick is complete (size == 4).
		*/
	lazy val isComplete: Boolean = size == Deal.CardsPerTrick

	/**
		* @return (optionally) the card play that was led to start the trick.
		*/
	lazy val led: Option[CardPlay] = plays.headOption

	/**
		* @return the most recent play of this Trick.
		* @throws CardException if this Trick has no plays
		*/
	lazy val last: CardPlay = if (plays.nonEmpty) plays.last else throw CardException(s"Trick: last: empty trick")

	/**
		* Add to the current Trick.
		*
		* @param play a card play which is to be added to the sequence of card plays.
		* @return a new Trick, with one more card play than this.
		*/
	//noinspection ScalaStyle
	def :+(play: CardPlay): Trick = Trick(if (isComplete || index == 0) index + 1 else index, plays :+ play)

	/**
		* @return true if the first card in this trick is an honor.
		*/
	lazy val isHonorLed: Boolean = led match {
		case Some(p) => p.isHonor
		case None => false
	}

	/**
		* Evaluate this Trick.
		*
		* @return a Double
		*/
	def evaluate: Double = _evaluate

	override def toString: String = s"T$index ${plays.mkString("{", ", ", "}")}"

	/**
		* This lazy val yields an optional Winner.
		* If this Trick is not yet started, then the result will be None, otherwise Some(winner).
		*/
	lazy val winner: Option[Winner] =
		if (started) {
			val winningPlay = plays.maxBy(p => if (p.suit == suit.get) Ace.priority - p.priority else 0)
			Some(Winner(winningPlay, isComplete))
		}
		else None

	/**
		* Enumerate the possible plays to follow the current play.
		*
		* @param whist the current game (only needed when the opening lead is being made)
		* @return a sequence of Trick instances, each based on:
		*         (1) the current trick if we are following;
		*         (2) a new trick if we are leading.
		*/
	def enumerateSubsequentPlays(whist: Whist): Seq[Trick] = enumerateSubsequentPlays(whist.deal, whist.openingLeader)

	/**
		* Determine if the declaring side still has a play left in this trick.
		*
		* @param directionNS true if NS is the declaring side.
		* @return true if fewer than three cards have been played; or if the leader is None, or leader belongs to the opposition.
		*/
	def declaringSideStillToPlay(directionNS: Boolean): Boolean = size < 3 || (leader match {
		case Some(x) => x % 2 == 1 ^ directionNS
		case None => true
	})

	/**
		* Determine the number of remaining moves that are required to build up sufficient tricks.
		*
		* @param directionNS  the direction of the declarer.
		* @param neededTricks the number of tricks required for the contract.
		* @param tricks       the current state of tricks
		* @return a minimum number of moves that will be required.
		*/
	def movesRequired(directionNS: Boolean, neededTricks: Int, tricks: Tricks): Int = {
		val movesByDeclarerThisTrick = if (declaringSideStillToPlay(directionNS)) 1 else 0
		val additionalTricksNeeded = neededTricks - (if (directionNS) tricks.ns else tricks.ew)
		(additionalTricksNeeded - movesByDeclarerThisTrick) * Deal.CardsPerTrick
	}

	private def enumerateSubsequentPlays(deal: Deal, leader: Int) = winner match {
		case Some(Winner(p, true)) =>
			enumerateLeads(deal, p.hand)
		case _ =>
			if (started)
				for (q <- deal.hands(Hand.next(last.hand)).choosePlays(deal, this)) yield this :+ q
			else
				enumerateLeads(deal, leader)
	}

	private def enumerateLeads(deal: Deal, leader: Int): Seq[Trick] = for (q <- chooseLeads(deal, leader)) yield Trick(index + 1, Seq(q))

	// TODO make private
	def chooseLeads(deal: Deal, leader: Int): Seq[CardPlay] = deal.hands(leader).longestSuit.choosePlays(deal, leader, FourthBest, None)

	lazy val value: Option[Double] = for (w <- winner; if w.complete) yield if (w.sameSide(0)) 1 else 0

	/**
		* The total number of cards played from the deal, including this trick.
		*
		* @return the total number of cards played.
		*/
	lazy val cardsPlayed: Int = Math.max((index - 1) * Deal.CardsPerTrick + size, 0)

	def output(output: Output, xo: Option[Deal] = None): Output =
		(output :+ s"T$index ") :+ (if (plays.nonEmpty) plays.last.output(output.copy, xo) else output.copy :+ "")

	private lazy val _evaluate = value.getOrElse(0.5)
}

/**
	* Class to represent the (current) winner of the trick.
	*
	* @param play     the current winning play.
	* @param complete true if the trick is complete.
	*/
case class Winner(play: CardPlay, complete: Boolean) {
	def sameSide(hand: Int): Boolean = Hand.sameSide(hand, play.hand)

	def priorityToBeat(hand: Int): Int = if (sameSide(hand)) Rank.lowestPriority else play.priority
}

/**
	* The play of a card.
	*
	* @param deal     the deal to which this play belongs (this is used solely for representing this play as an actual card).
	* @param hand     the index of this hand in the deal.
	* @param suit     rhe suit from which the card is to be played.
	* @param priority the priority of the sequence from which the card is to be played.
	*/
case class CardPlay(deal: Deal, hand: Int, suit: Suit, priority: Int) extends Ordered[CardPlay] with Outputable[Deal] {

	/**
		* @return true if this play can be validated. Basically, this means that no exception is thrown.
		*/
	lazy val validate: Boolean = asCard.suit == suit

	/**
		* @return true if the top card of the sequence indicated is at least a ten.
		*/
	lazy val isHonor: Boolean = Sequence.isHonor(priority)

	/**
		* Comparison between this and other.
		*
		* @param other the other CardPlay.
		* @return the usual less-than, equals, or greater-than.
		*/
	def compare(other: CardPlay): Int = Sequence.compare(priority, other.priority)

	/**
		* Method to determine whether this CardPlay beats the other CardPlay in whist-type game.
		*
		* @param other the other CardPlay.
		* @return true if this beats other.
		*/
	def beats(other: CardPlay): Boolean = compare(other) < 0

	/**
		* Yield the actual card to be played for this CardPlay (we arbitrarily choose the top card of a sequence)
		*
		* @return an actual Card.
		* @throws CardException if this CardPlay cannot be made from the given deal.
		*/
	lazy val asCard: Card =
		deal.hands(hand).holdings(suit).sequence(priority) match {
			case Some(s) => s.last
		case None =>
			throw CardException(s"CardPlay (deal=${deal.title}, hand=$hand, suit=$suit, priority=$priority) cannot find actual card.")
	}

	override def toString: String = s"Play: $hand $asCard"

	def output(output: Output, xo: Option[Deal]): Output = output :+ (Deal.name(hand) + ":" + asCard)
}

object CardPlay {

	implicit object LoggableCardPlay extends Loggable[CardPlay] with Loggables {
		val loggable: Loggable[CardPlay] = toLog4(CardPlay.apply, Seq("deal", "hand", "suit", "priority"))

		def toLog(t: CardPlay): String = loggable.toLog(t)
	}

}

object Trick {

	def create(index: Int, plays: CardPlay*): Trick = apply(index, plays)

	/**
		* Create an empty (non-) trick
		*/
	val empty: Trick = apply(0, Nil)

	implicit object LoggableTrick extends Loggable[Trick] with Loggables {
		def toLog(t: Trick): String = s"T${t.index} ${t.plays.mkString("{", ", ", "}")}"
	}

}

