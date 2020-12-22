/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.util.{Loggable, Loggables, Output, Outputable}

import scala.language.postfixOps

/**
  * The play of a card.
  *
  * @param deal     the deal to which this play belongs (this is used solely for representing this play as an actual card).
  * @param strain   optional suit which is the trump suit.
  * @param hand     the index of this hand in the deal.
  * @param suit     rhe suit from which the card is to be played.
  * @param priority the priority of the sequence from which the card is to be played.
  */
case class CardPlay(deal: Deal, strain: Option[Suit], hand: Int, suit: Suit, priority: Int) extends Outputable[Deal] {

  require(findSequence isDefined, s"impossible CardPlay: cannot locate card with suit: $suit, priority: $priority in hand $hand of ${deal.neatOutput}")

  /**
    * @return true if this play can be validated. Basically, this means that no exception is thrown.
    */
  lazy val validate: Boolean = asCard.suit == suit

  /**
    * NOTE: this seems to be totally arbitrary
    *
    * @return true if the top card of the sequence indicated is at least a ten.
    */
  lazy val isHonor: Boolean = Sequence.isHonor(priority)

  /**
    * Method to determine if this play is actually a ruff.
    *
    * @return true if ruffing else false.
    */
  lazy val isRuff: Boolean = strain contains suit

  /**
    * Method to determine if this play is a singleton in a plain suit against a trump contract, looking for a ruff.
    *
    * @param nCards the number of cards in the suit.
    * @return true if it's a stiff lead.
    */
  def isStiff(nCards: Int): Boolean = nCards == 1 && strain.nonEmpty && !strain.contains(suit)

  /**
    * Yield the actual card to be played for this CardPlay (we arbitrarily choose the top card of a sequence)
    *
    * @return an actual Card.
    * @throws CardException if this CardPlay cannot be made from the given deal.
    */
  lazy val asCard: Card =
    findSequence match {
      case Some(s) => s.last
      case None =>
        throw CardException(s"CardPlay (deal=${deal.title}, hand=$hand, suit=$suit, priority=$priority) cannot find actual card.")
    }

  /**
    * Find the sequence (lazily) that this CardPlay is from.
    */
  lazy val findSequence: Option[Sequence] = for (h <- deal.hands(hand).holdings.get(suit); s <- h.sequence(priority)) yield s

  override def toString: String = s"Play: $hand $asCard"

  def output(output: Output, xo: Option[Deal]): Output = output :+ (Hand.name(hand) + ":" + asCard)
}

object CardPlay {

  // NOTE: not used
  implicit object LoggableCardPlay extends Loggable[CardPlay] with Loggables {
    val loggable: Loggable[CardPlay] = toLog4((deal: Deal, hand: Int, suit: Suit, priority: Int) => CardPlay.apply(deal, None, hand, suit, priority), List("deal", "hand", "suit", "priority"))

    def toLog(t: CardPlay): String = loggable.toLog(t)
  }

}

