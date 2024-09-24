/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.util._

import scala.language.implicitConversions

/**
  * This class models a Whist hand (four Holdings, comprising thirteen cards).
  *
  * @param index    the index of this hand within the deal (North = 0).
  * @param holdings the four holdings (as a Map).
  */
case class Hand(index: Int, holdings: Map[Suit, Holding]) extends Outputable[Unit] with Quittable[Hand] with Playable[Hand] with Evaluatable with Validatable {

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
    val result = if (cardPlay.hand == index)
      this - (cardPlay.suit, priority)
    else
      this
    result promote(cardPlay.suit, priority)
  }

  /**
    * Method to determine possible discard plays.
    *
    * @param deal   the deal to which this Hand belongs.
    * @param strain the strain.
    * @param trick  the current state of the Trick.
    * @return a sequence of card plays.
    */
  def discardOrRuff(deal: Deal, strain: Option[Suit], trick: Trick): List[CardPlay] = {
    def strategy(suit: Suit, cards: Int): Strategy =
      strain map (_ == suit) map (b => if (b && cards > 0) Ruff else Discard) getOrElse Discard

    /**
      * Compare two plays returning:
      *   if both are ruffs or both are discards then we return true if the first play is less worthy than the second play (using current priority);
      *   otherwise, we return according to whether the first play is a ruff.
      * @param play1 the first play to compare.
      * @param play2 the second play to compare.
      * @return true if we want to choose the first play.
      */
    def comparePlays(play1: CardPlay, play2: CardPlay): Boolean = {
      val play1isRuff = play1.isRuff
      if (play1isRuff == play2.isRuff) play1.priority > play2.priority
      else play1isRuff
    }

    val plays = for {
      // TODO exclude the trump suit if there is one.
      // XXX get the holdings from each of the other suits.
      h: Holding <- holdings.flatMap { case (k, _) if trick.maybeSuit.contains(k) => None; case (_, v) => Some(v) }.toList
      // XXX determine the strategy for this holding (ruff or discard)
      s = strategy(h.suit, h.length)
      // XXX for each holding, get the lowest ranked card according to the strategy
      ps <- h.choosePlays(deal, strain, index, s, None)
    } yield ps

    // XXX sort the (one, two, or three) cards such that we choose ruffs first, then discards, always in order of least worthy first.
    plays sortWith comparePlays
  }

  /**
    * Choose the plays for this Hand, based on the prior plays of the given trick.
    *
    * @param deal  the deal from which the plays will be made.
    * @param trick the prior plays to the current trick.
    * @return a List[CardPlay].
    */
  def choosePlays(deal: Deal, trick: Trick, strain: Option[Suit]): List[CardPlay] =
    if (trick.started) {
      val holding = holdings(trick.maybeSuit.get)
      if (holding.isVoid) discardOrRuff(deal, strain, trick)
      else holding.chooseFollowSuitPlays(deal, strain, index, trick)
    }
    else throw CardException("choosePlays called with empty trick")

  /**
    * Method to get the count of the cards in this Hand.
    *
    * @return the sum of the cards in the holdings
    */
  lazy val nCards: Int = holdings.values.map(_.nCards).sum

  /**
    * Method to get the count of the cards in this Hand.
    *
    * @return the sum of the cards in the holdings
    */
  lazy val cards: Iterable[Card] = holdings.values.flatMap(_.cards)

  /**
    * Method to eliminate (play) a card from the given suit and sequence.
    * This method is called by the play method of Hand IF the hand matches.
    *
    * @param suit     the suit.
    * @param priority the priority of the sequence from which we take a card.
    * @return a new Hand, with one less card.
    */
  //noinspection ScalaStyle
  def -(suit: Suit, priority: Int): Hand =
    Hand(index, holdings + (suit -> (holdings(suit) - priority)))

  /**
    * Method to validate this Hand.
    *
    * @return true if this hand has 13 cards (clearly, this will only work when Hand is first created).
    */
  def validate: Boolean = nCards == Deal.CardsPerHand

  /**
    * Method to promote this Hand, for the given suit and the given priority.
    * Basically, this eagerly adjusts the holding by invoking promote on the holding for the given suit.
    *
    * @param suit     the given suit.
    * @param priority the priority for which we want to promote the suit.
    * @return a new eagerly promoted Hand.
    */
  def promote(suit: Suit, priority: Int): Hand =
    Hand(index, holdings + (suit -> holdings(suit).promote(priority)))

  /**
    * @return an eagerly promoted Hand.
    */
  def quit: Hand = _quit

  /**
    * Method to output this object (and, recursively, all of its children).
    * NOTE: never called.
    *
    * @param output the output to append to.
    * @param xo     an optional value of X, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit]): Output = {
    // TODO figure out why we can't just import SuitOrdering from Suit
    // NOTE: unused
    implicit object SuitOrdering extends Ordering[Suit] {
      override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
    }
    val keys = holdings.keys.toList.sorted.reverse
    output ++ (for (k <- keys) yield holdings(k).output(output.copy))
  }

  /**
    * NOTE: never called.
    *
    * @return a debug string for this Hand.
    */
  override def toString: String = {
    // TODO figure out why we can't just import SuitOrdering from Suit
    // NOTE: unused
    implicit object SuitOrdering extends Ordering[Suit] {
      override def compare(x: Suit, y: Suit): Int = -x.asInstanceOf[Priority].priority + y.asInstanceOf[Priority].priority
    }
    val keys = holdings.keys.toList.sorted.reverse
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
    val keys = holdings.keys.toList.sorted.reverse
    (for (k <- keys) yield holdings(k).neatOutput).mkString(" ")
  }

  lazy val asPBN: String = holdings.values.map(_.asPBN).mkString("", ".", "")

  private lazy val _evaluate = holdings.values.map(_.evaluate).sum

  // NOTE: only used for testing
  private lazy val _quit = Hand(index, for ((k, v) <- holdings) yield k -> v.quit)

  private lazy val _longestSuit = holdings.values.maxBy(_.length)
}

object Hand {

  /**
    * Create holdings from a list of Cards.
    * @param cs the list of Cards.
    * @return a Map of Suit->Holding
    */
  def createHoldings(cs: List[Card]): Map[Suit, Holding] = for ((suit, cards) <- cs.groupBy(c => c.suit)) yield (suit, Holding.create(suit, cards))

  /**
    * Create a Hand from a set of Strings representing Holdings.
    *
    * @param index index
    * @param ws    Strings representing holdings.
    * @return a Hand.
    */
  def from(index: Int, ws: String*): Hand = {
    val tuples = for (w <- ws; h = Holding.parseHolding(w)) yield h.suit -> h
    Hand(index, tuples.toMap)
  }

  /**
    * Get the index of the next hand.
    *
    * @param index current hand.
    * @return next hand.
    */
  def next(index: Int): Int = next(index, 1)

  /**
    * Get the index of a subsequent next hand.
    *
    * @param index current hand.
    * @param step  the number of moves clockwise around the table.
    * @return subsequent hand.
    */
  def next(index: Int, step: Int): Int = (index + step) % Deal.HandsPerDeal

  /**
    * Method to determine if the given hand is on the same side as the other hand.
    *
    * @param hand  one hand index.
    * @param other the other hand's index.
    * @return true if their difference is an even number.
    */
  def sameSide(hand: Int, other: Int): Boolean = (other - hand) % 2 == 0

  /**
    * Method to determine if the given hand is declarer or dummy.
    *
    * @param directionNS true if NS is declaring.
    * @param index       the hand index.
    * @return true if the hand is on declaring side.
    */
  def isDeclaringSide(directionNS: Boolean, index: Int): Boolean = (index % 2 == 1) ^ directionNS

//  implicit val z: Loggable[Hand] = (t: Hand) => t.neatOutput

  /**
    * This method must only be called with a valid index value.
    *
    * @param index an index between 0 and 3.
    * @return an appropriate name for the hand.
    */
  def name(index: Int): String = List("N", "E", "S", "W")(index)
}
