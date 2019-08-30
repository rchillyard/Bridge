/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.util._

import scala.language.implicitConversions

/**
  * A (non-empty) sequence of cards forming part of a Holding.
  *
  * NOTE: the priority of a sequence treats all three other Hands as being antagonists.
  * An optimization could be developed such that sequences are effectively combined for a partnership, not just a player.
  *
  * @param priority the number of higher-ranking cards in the suit.
  * @param cards    the cards.
  */
case class Sequence(priority: Int, cards: List[Card]) extends Evaluatable {

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
    * @return the lowest card of the sequence.
    *         This is the one that gets (arbitrarily) played when a sequence is used.
    */
  lazy val last: Card = cards.last

  /**
    * Method to promote this sequence by one.
    *
    * @return a new Sequence with the same cards but with a lower priority.
    */
  lazy val promote: Sequence = if (canPromote) Sequence(priority - 1, cards) else throw CardException(s"cannot promote priority $this ")

  /**
    * Merge this Sequence into a sequence of Sequences (ss).
    * If ss is empty or its last element cannot be combined with this, then we simply add this to ss.
    * Otherwise, we take the other elements of ss and add a combined Sequence from the last element and this.
    *
    * @param ss a sequence of Sequences.
    * @return a new sequence of Sequences.
    */
  def merge(ss: List[Sequence]): List[Sequence] = if (ss.nonEmpty && ss.last.canCombine(this)) ss.init :+ (ss.last ++ this) else ss :+ this

  /**
    * Method to concatenate two Sequences.
    *
    * @param s the input Sequence
    * @return the concatenation of this and s.
    */
  //noinspection ScalaStyle
  def ++(s: Sequence): Sequence = if (canCombine(s)) Sequence(priority, cards ++ s.cards) else throw CardException(s"cannot combine Sequences: $this and $s")

  /**
    * @return a String primarily for debugging purposes.
    */
  override def toString: String = s"${cards.map(_.rank).mkString("")}[$priority]"

  private lazy val canPromote = priority > 0

  private def canCombine(s: Sequence) = priority + length == s.priority

  /**
    * NOTE: this gets more and more optimistic as more tricks are turned.
    */
  private lazy val _evaluate = cards.length * math.pow(0.5, priority)
}

object Sequence {
  /**
    * Method to create a Sequence from a non-empty list of Cards.
    *
    * @param cs the list of Cards (must be non-empty).
    * @return a new Sequence.
    */
  def apply(cs: Seq[Card]): Sequence = apply(cs.head.priority, cs.toList)

  /**
    * @return true if the top card of the sequence with the given priority is at least a ten.
    */
  def isHonor(priority: Int): Boolean = priority <= Rank.honorPriority

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

  /**
    * An ordering for a Sequence.
    * Lower values of priority precede higher values.
    */
  implicit object SequenceOrdering extends Ordering[Sequence] {
    override def compare(x: Sequence, y: Sequence): Int = x.priority - y.priority
  }

  /**
    * An loggable for a Sequence.
    * Lower values of priority precede higher values.
    */
  implicit object LoggableSequence extends Loggable[Sequence] with Loggables {
    implicit val cardSequenceLoggable: Loggable[List[Card]] = listLoggable[Card]
    // NOTE: that, for this particular apply method, we have to specify the fields we need.
    val loggableSequence: Loggable[Sequence] = toLog2(Sequence.apply, List("priority", "cards"))

    def toLog(t: Sequence): String = loggableSequence.toLog(t)
  }

}

/**
  * This class models a holding in a suit.
  * A Holding is made up of a sequence of Sequences for a particular suit.
  * Additionally, a Holding keeps track of lazily implemented promotions.
  *
  * @param sequences  the sequences (expected to be in order of rank).
  * @param suit       the suit of this holding.
  * @param promotions a list of promotions that should be applied on quitting a trick.
  */
case class Holding(sequences: List[Sequence], suit: Suit, promotions: List[Int] = Nil)
  extends Outputable[Unit] with Quittable[Holding] with Evaluatable with Removable {

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
  lazy val cards: List[Card] = for (s <- sequences; c <- s.cards) yield c

  /**
    * @return the effective number of cards.
    */
  lazy val nCards: Int = cards.size

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
    * All possible plays are returned, but the order in which they occur is dependent on the Strategy chosen.
    *
    * @param deal  the deal to which these plays will belong.
    * @param hand  the index of the Hand containing this Holding.
    * @param trick the current state of this trick (i.e. the prior plays).
    * @return a sequence of all possible plays, starting with the ones most suited to the appropriate strategy.
    */
  def choosePlays(deal: Deal, hand: Int, trick: Trick): List[CardPlay] = {
    // TODO eliminate this method: it never results in Discard
    def suitMatches(x: Strategy) = trick.suit match {
      case Some(`suit`) => x;
      case _ => Discard
    }

    // XXX Determine the Strategy to be used when choosePlays is called.
    lazy val strategy: Strategy = trick.size match {
      case 0 => if (hasHonorSequence) LeadHigh else FourthBest
      case 1 => suitMatches(if (trick.isHonorLed || realSequences.nonEmpty) Cover else Duck)
      case 2 => suitMatches(Finesse) // XXX becomes WinIt if card to beat isn't an honor
      case 3 => suitMatches(Cover)
      case x => throw CardException(s"too many prior plays: $x")
    }
    choosePlays(deal, hand, strategy, trick.winner)
  }

  /**
    * For now, we ignore strategy which is only used to ensure that we try the likely more successful card play first.
    *
    * @param deal          the deal to which these plays will belong.
    * @param hand          the index of this Hand (N, E, S, W).
    * @param strategy      the recommended strategy.
    * @param currentWinner the play currently winning the trick.
    * @return a sequence of CardPlay objects.
    */
  def choosePlays(deal: Deal, hand: Int, strategy: Strategy, currentWinner: Option[Winner]): List[CardPlay] = {
    def createPlay(priority: Int): CardPlay = CardPlay(deal, hand, suit, priority)

    lazy val priorityToBeat = (currentWinner map (_.priorityToBeat(hand))).getOrElse(Rank.lowestPriority)
    lazy val isPartnerWinning: Boolean = currentWinner exists (_.partnerIsWinning(hand))
    strategy match {
      case Discard =>
        sequences.lastOption.toList map (s => createPlay(s.priority))
      case Finesse if priorityToBeat > Rank.honorPriority =>
        choosePlays(deal, hand, WinIt, currentWinner)
      case WinIt if isPartnerWinning =>
        chooseNonDiscardPlays(createPlay, Duck, priorityToBeat)
      case _ =>
        chooseNonDiscardPlays(createPlay, strategy, priorityToBeat)
    }
  }

  private def chooseNonDiscardPlays(createPlay: Int => CardPlay, strategy: Strategy, priorityToBeat: Int) = {
    // XXX this function is used to sort the possible plays according to which fits the given strategy best (smallest resulting Int)
    def sortFunction(play: CardPlay): Int = applyStrategy(play, strategy, priorityToBeat)

    (for (s <- sequences) yield createPlay(s.priority)).sortBy(sortFunction)
  }

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
    * Method to remove (i.e. play) a card from this Holding.
    *
    * @param priority the sequence from which the card will be played.
    * @return a new Holding with the sequence either truncated or eliminated entirely.
    */
  //noinspection ScalaStyle
  def -(priority: Int): Holding = {
    val sos: List[Option[Sequence]] = for (s <- sequences) yield if (s.priority == priority) s.truncate else Some(s)
    Holding(sos.flatten, suit, promotions)
  }

  /**
    * @return a String primarily for debugging purposes.
    */
  override def toString: String = s"{$suit: ${sequences.mkString(", ")}} " + (if (promotions.nonEmpty) promotions.mkString(", ") else "(clean)")

  /**
    * @return a neat representation of this Sequence.
    */
  lazy val neatOutput: String = s"$suit${Holding.ranksToString(cards map (_.rank))}"

  /**
    * @return a neat representation of this Sequence (without suit symbol).
    */
  lazy val asPBN: String = s"${Holding.ranksToString(cards map (_.rank))}"

  /**
    * Output this Sequence to an Output.
    *
    * @param output the output to append to.
    * @param xo     an optional value of Unit, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output = output :+ suit.toString :+ Holding.ranksToString(cards map (_.rank))

  /**
    * Method to assess the given strategy in the current situation.
    *
    * @param play          a potential card play.
    * @param strategy      the required strategy.
    * @param currentWinner the priority of the card play which is currently winning this trick.
    * @return a relatively low number (e.g. 0) if this matches the given strategy, otherwise a high number.
    */
  //	private
  def applyStrategy(play: CardPlay, strategy: Strategy, currentWinner: Int): Int = {
    lazy val rank = 2 * Rank.lowestPriority - play.priority // XXX the rank of the played card plus 14

    def applyPotentialWinStrategy =
      if (strategy.conditional)
        currentWinner - play.priority // XXX prefer the card that wins by the slimmest margin (always positive)
      else if (strategy.win)
        play.priority // XXX play high.
      else
        rank // XXX play low.

    if (play.suit != suit)
      rank // XXX discard situation: prefer the lowest ranking card. Is this condition ever used?
    else if (play.priority < currentWinner) // XXX can we win this trick if we want to?
      applyPotentialWinStrategy
    else
      rank // XXX play low.
  }

  private lazy val _quit = {

    def applyPromotions(sequence: Sequence): Sequence = {
      val promotion = promotions.count(_ < sequence.priority)
      Sequence(sequence.priority - promotion, sequence.cards)
    }

    val ss: List[Sequence] = sequences map applyPromotions
    Holding(ss.foldLeft[List[Sequence]](Nil)((r, s) => s.merge(r)), suit, Nil)
  }

  private lazy val hasHonorSequence: Boolean = realSequences exists (_.isHonor)

  private lazy val realSequences = sequences filter (_.cards.lengthCompare(1) > 0)

  //noinspection ScalaUnusedSymbol
  // NOTE: never called
  private def canWin(priorPlays: List[CardPlay]): Boolean = if (priorPlays.nonEmpty && !isVoid) priorPlays.min.priority > sequences.head.priority else true

  private lazy val maybeSuit: Option[Suit] = cards.headOption map (_.suit)

  private lazy val _evaluate: Double = {
    // TODO Do this properly but, for now, I'm going to use iteration and var !!
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

  /**
    * Create a new Holding from a suit and a variable number of Ranks.
    *
    * @param suit  the suit.
    * @param ranks a variable number of Ranks.
    * @return a new Holding.
    */
  def apply(suit: Suit, ranks: Rank*): Holding = {
    val cards = ranks map (rank => Card(suit, rank))
    val cXsXm = (for ((c, i) <- cards.zipWithIndex) yield i - c.priority -> c).groupBy(_._1)
    val ss = cXsXm.values map (cXs => Sequence(cXs.map(_._2)))
    apply(ss.toList.sorted, suit, Nil)
  }

  /**
    * Create a new Holding from a suit and a String representing the Ranks.
    *
    * @param suit  the suit.
    * @param ranks the Ranks.
    * @return a new Holding.
    */
  def apply(suit: Suit, ranks: String): Holding = create(Card.parser.parseRanks(ranks).toList, suit)

  /**
    * Implicit converter from a String to a Holding.
    *
    * @param s the String made up of (abbreviated) suit and ranks.
    * @return a new Holding.
    */
  implicit def parseHolding(s: String): Holding = create(Card.parser.parseRanks(s.tail).toList, Suit(s.head))

  /**
    * An ordering for Ranks.
    * Lower priorities precede higher priorities.
    * TODO merge with duplicate code.
    */
  implicit object RankOrdering extends Ordering[Rank] {
    override def compare(x: Rank, y: Rank): Int = -x.priority + y.priority
  }

  // CONSIDER merge the two create methods
  def create(suit: Suit, cards: Seq[Card]): Holding = apply(suit, (cards map (_.rank)).sorted.reverse: _*)

  def create(ranks: Seq[Rank], suit: Suit): Holding = apply(suit, ranks.sorted.reverse: _*)

  def ranksToString(ranks: Seq[Rank]): String = if (ranks.nonEmpty) ranks.mkString("", "", "") else "-"

  implicit object LoggableHolding extends Loggable[Holding] with Loggables {
    def toLog(t: Holding): String = t.neatOutput
  }

}

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
    * @param trick the current state of the Trick.
    * @return a sequence of card plays.
    */
  def discard(deal: Deal, trick: Trick): List[CardPlay] = {
    def suitsMatch(k: Suit) = trick.suit match {
      case Some(`k`) => true;
      case _ => false
    }

    val plays = for {
      // XXX get the holdings from each of the other suits.
      h <- holdings.flatMap { case (k, v) => if (suitsMatch(k)) None else Some(v) }.toList
      // XXX for each holding, get the lowest ranked card
      ps <- h.choosePlays(deal, index, Discard, None)
    } yield ps

    // XXX sort the (one, two, or three) cards such that we try the least worthy first.
    // NOTE: that we use current priority, not rank here.
    plays.sortBy(-_.priority)
  }

  /**
    * Choose the plays for this Hand, based on the prior plays of the given trick.
    *
    * @param deal  the deal from which the plays will be made.
    * @param trick the prior plays to the current trick.
    * @return a List[CardPlay].
    */
  def choosePlays(deal: Deal, trick: Trick): List[CardPlay] =
    if (trick.started) {
      val holding = holdings(trick.suit.get)
      if (holding.isVoid) discard(deal, trick)
      else holding.choosePlays(deal, index, trick)
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
    * CONSIDER eliminating: not used.
    *
    * @return the index of the next hand in sequence around the table.
    */
  lazy val next: Int = Hand.next(index)

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
    val keys = holdings.keys.toList.sorted.reverse
    output ++ (for (k <- keys) yield holdings(k).output(output.copy))
  }

  override def toString: String = {
    // TODO figure out why we can't just import SuitOrdering from Suit
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
    * NOTE: This doesn't really make sense.
    *
    * @param deal  deal
    * @param index index
    * @param cs    cards
    * @return a Hand.
    */
  def apply(deal: Deal, index: Int, cs: List[Card]): Hand = Hand(index, createHoldings(cs))

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

  implicit val z: Loggable[Hand] = (t: Hand) => t.neatOutput
}
