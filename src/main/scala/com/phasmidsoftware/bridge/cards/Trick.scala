/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

/**
  * Module which includes Trick and Winner
  */

import com.phasmidsoftware.util.{Output, Outputable}

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

  lazy val maybeSuit: Option[Suit] = plays.headOption.map(_.suit)

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
  lazy val history: List[Trick] = maybePrior match {
    case None => List(this)
    case Some(Trick.empty) => List(this)
    case Some(t) => t.history :+ this
  }

  /**
    * This lazy val yields an optional Winner.
    * If this Trick is not yet started, then the result will be None, otherwise Some(winner).
    */
  lazy val winner: Option[Winner] =
    if (started) {
      val winningPlay = plays maxBy score
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

  private def followingSuit(p: CardPlay) = maybeSuit contains p.suit

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
    val requiredMoves = (neededTricks - (if (directionNS) tricks.ns else tricks.ew)) * Deal.CardsPerTrick
    moves >= requiredMoves || (declaringSideCanWin(directionNS) && (plays.size >= requiredMoves - moves))
  }

  /**
    * NOTE: this doesn't look right
    *
    * TODO: this entire mechanism of generating plays needs a complete re-write!
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

  private def leadStrategy(s: Suit, h: Holding, strain: Option[Suit]): Strategy = h.nCards match {
    case 0 => Invalid
    case 1 if strain.nonEmpty && !strain.contains(s) => Stiff
    case _ => StandardOpeningLead
  }

  // TODO make private
  def chooseLeads(deal: Deal, leader: Int, strain: Option[Suit]): List[CardPlay] = {
    val z: List[(CardPlay, Int)] = for {(s, h) <- deal.hands(leader).holdings.toList
                                        strategy = leadStrategy(s, h, strain)
                                        p <- h.choosePlays(deal, strain, leader, strategy, None)}
    yield p -> h.nCards
    // TODO incorporate this into the code
    val _: List[(Suit, List[(CardPlay, Int)])] = z.groupBy { case (p, _) => p.suit }.toList
    val (q, _) = z.sortWith((x, _) => x._1.isStiff(x._2)).sortBy(x => -x._2).unzip
    q
  }

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

object Trick {

  def create(index: Int, plays: CardPlay*): Trick = apply(index, plays.toList, None)

  /**
    * Create an empty (non-) trick
    */
  val empty: Trick = apply(0, Nil, None)
  //
  //  implicit object LoggableTrick extends Loggable[Trick] with Loggables {
  //    def toLog(t: Trick): String = s"T${t.index} ${t.plays.mkString("{", ", ", "}")}"
  //  }

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

  // TODO this looks a bit suspicious, but it is indeed used to prioritize plays!
  def partnerIsWinning(hand: Int): Boolean = play.isHonor && sameSide(hand)

  // NOTE: the following logical looking alternative doesn't work well.
  //    (play.isHonor || play.isRuff) && sameSide(hand)

  def isDeclaringSide(NS: Boolean): Boolean = Hand.isDeclaringSide(NS, play.hand)
}

object Winner {
  def isDeclaringSideWinning(maybeWinner: Option[Winner], NS: Boolean): Boolean = maybeWinner match {
    case Some(w) => w.isDeclaringSide(NS)
    case None => false
  }
}

