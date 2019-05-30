/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmid.laScala.Shuffle
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

/**
  * Class to describe a Deal.
  * In particular, this class describes the current state of a Deal, so there will not always be 52 cards in it.
  * The Holdings are typically lazily promoted--for an eagerly promoted Deal, you should invoke quit.
  *
  * @param title    the title of this Deal.
  * @param holdings the holdings of the four Hands of this Deal.
  */
case class Deal(title: String, holdings: Map[Int, Map[Suit, Holding]]) extends Outputable[Unit] with Quittable[Deal] with Playable[Deal] with Evaluatable {

  /**
    * Method to return a sequence representing the four hands of this Deal.
    */
  lazy val hands: Seq[Hand] = (for ((i, hs) <- holdings) yield Hand(this, i, hs)).toSeq

  /**
    * @return an eagerly promoted Deal.
    */
  def quit: Deal = _quit

  lazy val north: Hand = n

  lazy val east: Hand = e

  lazy val south: Hand = s

  lazy val west: Hand = w

  /**
    * Play a card from this Playable object.
    *
    * @param cardPlay the card play.
    * @return a new Playable.
    */
  def play(cardPlay: CardPlay): Deal = Deal(title, hands map (_.play(cardPlay)))

  /**
    * Evaluate the N and S hands heuristically.
    *
    * @return a number which corresponds to the trick-taking ability of the N/S hands.
    */
  def evaluate: Double = _evaluate

  /**
    * @return the number of cards remaining in this Deal.
    */
  lazy val cards: Int = hands map (_.cards) sum

  /**
    * Promote the given Hand in the given suit, below the given priority and return a new Deal.
    *
    * NOTE: the concept of promotion used here views the four players independently.
    * CONSIDER: it probably makes sense to have another value of priority in a Holding which takes into account
    * the fact that two opposite players are partners.
    *
    * @param hand     the Hand.
    * @param suit     the Suit.
    * @param priority the priority.
    * @return a new Deal where the suit holding in the hand has been promoted (if appropriate).
    */
  def promote(hand: Hand, suit: Suit, priority: Int): Deal = Deal(title, hands map (h => if (h == hand) h else h.promote(suit, priority)))

  /**
    * Output this Deal.
    *
    * @param output the output to append to.
    * @param xo     an optional value of Unit, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit]): Output =
    (output :+ title).insertBreak ++ outputHand("North", n) ++ outputHand("East", e) ++ outputHand("South", s) ++ outputHand("West", w)

  /**
    * Neat output.
    */
  lazy val neatOutput: String = s"Deal $title ($cards)\n${hands.map(_.neatOutput)}"

  override def toString: String = s"Deal $title ($cards)\n${hands.mkString("\n")}"

  /**
    * Play a trick (made up of four card plays).
    *
    * NOTE: only invoked by unit tests.
    *
    * @param trick the card play from each of the four hands.
    */
  private[cards] def playAll(trick: Trick): Deal = Deal(title, hands map (_.playAll(trick)))

  private val Seq(n, e, s, w) = hands

  private lazy val _quit = Deal(title, for ((k, v) <- holdings) yield k -> (for ((s, h) <- v) yield s -> h.quit))

  private def outputHand(name: String, hand: Hand): Output = (Output(s"$name:\t") :+ hand.neatOutput).insertBreak

  private lazy val _evaluate = hands.sliding(1, 2).flatten.map(_.evaluate).sum
}

object Deal {
  /**
    * The number of Cards per Hand: 13
    */
  val CardsPerHand: Int = 13

  /**
    * The number of Hands per Deal: 4
    */
  val HandsPerDeal: Int = 4

  /**
    * The number of Cards per Deal: 52
    */
  val CardsPerDeal: Int = CardsPerHand * HandsPerDeal

  /**
    * The number of Tricks per Deal: 13
    */
  val TricksPerDeal: Int = 13

  /**
    * The number of Cards per Trick: 4
    */
  val CardsPerTrick: Int = 4

  def apply(title: String, hands: Seq[Hand]): Deal = apply(title, (for (h <- hands) yield h.index -> h.holdings).toMap)

  /**
    * Construct a Deal from a sequence of Cards.
    *
    * TODO this should not need openingLeader.
    *
    * @param title a title for the new Deal.
    * @param cs    the cards dealt in sequence.
    * @return a new Deal.
    */
  def fromCards(title: String, cs: Seq[Card]): Deal =
    new Deal(title, (for ((cs, index) <- cs.grouped(CardsPerHand).zipWithIndex) yield index -> Hand.createHoldings(cs)).toMap)

  /**
    * Construct a Deal from a sequence of Cards.
    *
    * @param title a title for the Deal.
    * @param seed  a seed for the random number generator,
    * @return
    */
  def apply(title: String, seed: Long = System.nanoTime()): Deal = {
    val newDeck: Seq[Card] =
      for (s <- Seq(Spades, Hearts, Diamonds, Clubs); r <- Seq(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)) yield Card(s, r)
    val shuffler = Shuffle[Card](seed)
    fromCards(title, shuffler(newDeck))
  }

  /**
    * This method must only be called with valid hand value.
    *
    * @param hand an index between 0 and 3.
    * @return an appropriate name for the hand.
    */
  def name(hand: Int): String = Seq("N", "E", "S", "W")(hand)
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
  lazy val asCard: Card = deal.hands(hand).holdings(suit).sequence(priority) match {
    case Some(s) => s.head
    case None => throw CardException(s"CardPlay (deal=${deal.title}, hand=$hand, suit=$suit, priority=$priority) cannot find actual card.")
  }

  override def toString: String = s"Play: $hand $asCard"

  def output(output: Output, xo: Option[Deal]): Output = output :+ (Deal.name(hand) + ":" + asCard)
}

/**
  * A set of 0 to 4 card plays which describe the state of the current trick.
  *
  * NOTE: we extend Outputable[Deal] because that gives the type of the second (optional) parameter to the output method.
  *
  * TODO move Trick into Whist
  *
  * @param index the position of this trick in sequence, starting with zero.
  * @param plays the sequence of plays (in sequence).
  */
case class Trick(index: Int, plays: Seq[CardPlay]) extends Outputable[Deal] with Evaluatable {

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
  def :+(play: CardPlay): Trick = Trick(if (isComplete) index + 1 else index, plays :+ play)

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

  lazy val value: Option[Double] = for (w <- winner; if w.complete) yield if (w.sameSide(0)) 1 else 0

  /**
    * The total number of cards played from the deal, including this trick.
    *
    * @return the total number of cards played.
    */
  lazy val cardsPlayed: Int = index * Deal.CardsPerTrick + size

  def output(output: Output, xo: Option[Deal] = None): Output =
    (output :+ s"T$index ") :+ (if (plays.nonEmpty) plays.last.output(output.copy, xo) else output.copy :+ "")

  private lazy val _evaluate = value.getOrElse(0.5)
}

object Trick {

  def create(index: Int, plays: CardPlay*): Trick = apply(index, plays)
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