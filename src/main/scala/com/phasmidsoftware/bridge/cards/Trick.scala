/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.util.{Loggable, Loggables, Output, Outputable}

import scala.language.postfixOps

/**
  * A set of 0 to 4 card plays which describe the state of the current trick.
  *
  * NOTE: we extend Outputable[Deal] because that gives the type of the second (optional) parameter to the output method.
  *
  * CONSIDER removing maybePrior
  *
  * @param index      the position of this trick in sequence, starting with one.
  * @param plays      the sequence of plays (in sequence).
  * @param maybePrior an optional previous trick
  */
case class Trick(index: Int, plays: List[CardPlay], maybePrior: Option[Trick]) extends Outputable[Deal] with Evaluatable {

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
    * CONSIDER this seems odd that we always pass plays into the new Trick.
    *
    * @param play a card play which is to be added to the sequence of card plays.
    * @return a new Trick, with one more card play than this.
    */
  //noinspection ScalaStyle
  def :+(play: CardPlay): Trick =
    if (isComplete || index == 0) Trick(index + 1, List(play), if (index == 0) None else Some(this))
    else if (next contains play.hand) Trick(index, plays :+ play, maybePrior)
    else throw CardException(s"play $play cannot be added to this trick: $this ")

  //		Trick(if (isComplete || index == 0) index + 1 else index, plays :+ play, if (isComplete) Some(this) else None)

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

  override def toString: String = s"T$index ${leader.map(_.toString).getOrElse("")} ${plays.map(_.asCard).mkString("{", ", ", "}")}"

  /**
    * Refactor this
    */
  lazy val history: Seq[Trick] = maybePrior match {
    case None => Seq(this)
    case Some(Trick.empty) => Seq(this)
    case Some(t) => t.history :+ this
  }

  /**
    * This lazy val yields an optional Winner.
    * If this Trick is not yet started, then the result will be None, otherwise Some(winner).
    */
  lazy val winner: Option[Winner] =
    if (started) {
      val winningPlay = plays maxBy score
      //      println(s"$this: winner = $winningPlay")
      Some(Winner(winningPlay, isComplete))
    }
    else None

  /**
    * This method yields a score which can be used to determine the winning play.
    *
    * @param p a play.
    * @return an Int which will be large for a winning play and small for a losing play.
    */
  private def score(p: CardPlay) = {
    val base = if (followingSuit(p)) Rank.lowestPriority else if (p.isRuff) 2 * Rank.lowestPriority else 0
    base - p.priority
  }

  private def followingSuit(p: CardPlay) = suit contains p.suit

  /**
    * Enumerate the possible plays to follow the current play.
    *
    * TODO move this into Whist?
    *
    * @param whist the current game (only needed when the opening lead is being made) ???
    * @return a sequence of Trick instances, each based on:
    *         (1) the current trick if we are following;
    *         (2) a new trick if we are leading.
    */
  def enumerateSubsequentPlays(whist: Whist): List[Trick] = enumerateSubsequentPlays(whist.deal, whist.openingLeader, whist.strain) //.invariant(ts => ts.nonEmpty)

  /**
    * Determine if the declaring side still has a play left in this trick.
    *
    * @param directionNS true if NS is the declaring side.
    * @return true if fewer than three cards have been played; or if the leader is None, or leader belongs to the opposition.
    */
  def declaringSideStillToPlay(directionNS: Boolean): Boolean = size < 3 || (leader match {
    case Some(x) => !Hand.isDeclaringSide(directionNS, x)
    case None => true
  })

  /**
    * Determine if the declaring side still has a play left in this trick.
    *
    * @param directionNS true if NS is the declaring side.
    * @return true if fewer than three cards have been played; or if the leader is None, or leader belongs to the opposition.
    */
  def declaringSideCanWin(directionNS: Boolean): Boolean = declaringSideStillToPlay(directionNS) || Winner.isDeclaringSideWinning(winner, directionNS)

  /**
    * Determine the number of remaining moves that are required to build up sufficient tricks.
    *
    * @param directionNS  the direction of the declarer.
    * @param neededTricks the number of tricks required for the contract.
    * @param tricks       the current state of tricks
    * @return a minimum number of moves that will be required.
    */
  def sufficientMovesRemaining(moves: Int, directionNS: Boolean, neededTricks: Int, tricks: Tricks): Boolean = {
    val canWin = declaringSideCanWin(directionNS)
    val additionalTricksNeeded = neededTricks - (if (directionNS) tricks.ns else tricks.ew)
    val z = additionalTricksNeeded * Deal.CardsPerTrick
    val result = moves >= z || (canWin && (plays.size >= z - moves))
    //    if (!result)
    //      println(s"impossible: insufficientMovesRemaining: $this, moves=$moves, directionNS=$directionNS, neededTricks=$neededTricks, tricks=$tricks")
    result
  }

  /**
    * NOTE: this doesn't look right
    *
    * @param deal the deal.
    * @param leader the opening leader.
    * @param strain the trump suit, if any.
    * @return a list of Tricks.
    */
  private def enumerateSubsequentPlays(deal: Deal, leader: Int, strain: Option[Suit]) = // if (deal.nCards<4) List(forcedPlay(deal, leader)) else
    winner match {
      case Some(Winner(p, true)) =>
        enumerateLeads(deal, p.hand, strain) // XXX enumerate leads, given a complete trick with an actual winner
      case _ =>
        if (started)
          for (q <- deal.hands(next.get).choosePlays(deal, this, strain)) yield this :+ q
        else
          enumerateLeads(deal, leader, strain) // XXX: enumerate leads, starting from the null trick.
    }

  private def enumerateLeads(deal: Deal, leader: Int, strain: Option[Suit]) = for (q <- chooseLeads(deal, leader, strain)) yield Trick(index + 1, List(q), Some(this))

  // TODO make private
  def chooseLeads(deal: Deal, leader: Int, strain: Option[Suit]): List[CardPlay] = deal.hands(leader).longestSuit.choosePlays(deal, strain, leader, FourthBest, None)

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

  // TODO this looks very suspicious
  def partnerIsWinning(hand: Int): Boolean = play.isHonor && sameSide(hand)

  def isDeclaringSide(NS: Boolean): Boolean = Hand.isDeclaringSide(NS, play.hand)
}

object Winner {
  def isDeclaringSideWinning(maybeWinner: Option[Winner], NS: Boolean): Boolean = maybeWinner match {
    case Some(w) => w.isDeclaringSide(NS)
    case None => false
  }
}

/**
  * The play of a card.
  *
  * @param deal     the deal to which this play belongs (this is used solely for representing this play as an actual card).
  * @param strain   optional suit which is the trump suit.
  * @param hand     the index of this hand in the deal.
  * @param suit     rhe suit from which the card is to be played.
  * @param priority the priority of the sequence from which the card is to be played.
  */
case class CardPlay(deal: Deal, strain: Option[Suit], hand: Int, suit: Suit, priority: Int) extends Ordered[CardPlay] with Outputable[Deal] {

  require(findSequence isDefined, s"impossible CardPlay: cannot locate card with suit: $suit, priority: $priority in hand $hand of ${deal.neatOutput}")

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
    * Method to determine if this play is actually a ruff.
    *
    * @return true if ruffing else false.
    */
  lazy val isRuff: Boolean = strain contains suit

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

  implicit object LoggableCardPlay extends Loggable[CardPlay] with Loggables {
    val loggable: Loggable[CardPlay] = toLog4((deal: Deal, hand: Int, suit: Suit, priority: Int) => CardPlay.apply(deal, None, hand, suit, priority), List("deal", "hand", "suit", "priority"))

    def toLog(t: CardPlay): String = loggable.toLog(t)
  }

}

object Trick {

  def create(index: Int, plays: CardPlay*): Trick = apply(index, plays.toList, None)

  /**
    * Create an empty (non-) trick
    */
  val empty: Trick = apply(0, Nil, None)

  implicit object LoggableTrick extends Loggable[Trick] with Loggables {
    def toLog(t: Trick): String = s"T${t.index} ${t.plays.mkString("{", ", ", "}")}"
  }

}

