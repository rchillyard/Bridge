/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.gambit.util.{Output, Outputable}

import scala.language.postfixOps

/**
  * The play of a card.
  *
  * Note: instead of storing the full `Deal`, we store only the resolved `Card`
  * at construction time. This avoids retaining a `Deal` reference per card play
  * in the search tree, significantly reducing memory pressure during double-dummy
  * analysis, while still allowing correct display of historical plays.
  *
  * @param card        the actual card being played.
  * @param strain   optional suit which is the trump suit.
  * @param hand     the index of this hand in the deal.
  * @param suit        the suit from which the card is to be played.
  * @param priority the priority of the sequence from which the card is to be played.
  */
case class CardPlay(card: Card, strain: Option[Suit], hand: Int, suit: Suit, priority: Int) extends Outputable[Deal] {

  /**
    * @return true if this play can be validated.
    */
  lazy val validate: Boolean = card.suit == suit

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
  def isStiff(nCards: Int): Boolean =
    nCards == 1 && strain.nonEmpty && !strain.contains(suit)

  override def toString: String = s"Play: ${Hand.name(hand)} $card"

  def output(output: Output, xo: Option[Deal]): Output =
    output :+ (Hand.name(hand) + ":" + card)
}

object CardPlay {
  /**
    * Construct a CardPlay, resolving the card immediately from the deal.
    *
    * @param deal     the deal at the time of play (used only to resolve the card).
    * @param strain   optional trump suit.
    * @param hand     the index of this hand in the deal.
    * @param suit     the suit from which the card is to be played.
    * @param priority the priority of the sequence from which the card is to be played.
    * @return a new CardPlay.
    */
  def apply(deal: Deal, strain: Option[Suit], hand: Int, suit: Suit, priority: Int): CardPlay = {
    val maybeCard = for (h <- deal.hands(hand).holdings.get(suit); s <- h.sequence(priority)) yield s.last
    val card = maybeCard.getOrElse(throw CardException(
      s"impossible CardPlay: cannot locate card with suit: $suit, priority: $priority in hand $hand of ${deal.neatOutput}"))
    CardPlay(card, strain, hand, suit, priority)
  }
}