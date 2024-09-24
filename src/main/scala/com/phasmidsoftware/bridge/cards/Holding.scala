/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.util._

import scala.language.implicitConversions

/**
  * This class models a holding in a suit.
  * A Holding is made up of a sequence of Sequences for a particular suit.
  * Additionally, a Holding keeps track of lazily implemented promotions.
  *
  * @param sequences  the sequences (expected to be in order of rank).
  * @param suit       the suit of this holding.
  * @param promotions a list of promotions that should be applied on quitting a trick.
  *                   CONSIDER eliminating the list of promotions if holding is void.
  */
case class Holding(sequences: List[Sequence], suit: Suit, promotions: List[Int] = Nil)
  extends Outputable[Unit] with Quittable[Holding] with Cooperative[Holding] with Reprioritizable[Holding] with Evaluatable with Removable {

  require(isVoid || maybeSuit.get == suit)

  /**
    * @return the number of sequences in this Holding
    */
  lazy val size: Int = sequences.size

  /**
    * @return the number of cards in this Holding (i.e. the suit length)
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
    * This Holding corresponds to the suit of trick and is never empty.
    *
    * All possible plays are returned, but the order in which they occur is dependent on the Strategy chosen.
    *
    * @param deal  the deal to which these plays will belong.
    * @param hand  the index of the Hand containing this Holding.
    * @param trick the current state of this trick (i.e. the prior plays).
    * @return a sequence of all possible plays, starting with the ones most suited to the appropriate strategy.
    */
  def chooseFollowSuitPlays(deal: Deal, strain: Option[Suit], hand: Int, trick: Trick): List[CardPlay] =
    choosePlays(deal, strain, hand, getStrategyForFollowingSuit(trick), trick.winner)

  /**
    * For now, we ignore strategy which is only used to ensure that we try the likely more successful card play first.
    *
    * @param deal          the deal to which these plays will belong.
    * @param hand          the index of this Hand (N, E, S, W).
    * @param strain        the (optional) trump suit.
    * @param strategy      the recommended strategy.
    * @param currentWinner the play currently winning the trick.
    * @return a sequence of CardPlay objects.
    */
  def choosePlays(deal: Deal, strain: Option[Suit], hand: Int, strategy: Strategy, currentWinner: Option[Winner]): List[CardPlay] = {
    def createPlay(priority: Int): CardPlay = CardPlay(deal, strain, hand, suit, priority)

    lazy val priorityToBeat = (currentWinner map (_.priorityToBeat(hand))).getOrElse(Rank.lowestPriority)
    lazy val isPartnerWinning = currentWinner exists (_.partnerIsWinning(hand))
    strategy match {
      case StandardOpeningLead if hasHonorSequence =>
        choosePlays(deal, strain, hand, LeadTopOfSequence, currentWinner)
      case StandardOpeningLead => // "Fourth best" although that's not followed precisely--we just lead low
        chooseLeadSuitPlays(createPlay, strategy)
      case Ruff if isPartnerWinning =>
        choosePlays(deal, strain, hand, Discard, currentWinner)
      case Ruff | Discard =>
        // NOTE: these cards will be ordered appropriately by the caller.
        sequences.lastOption.toList map (s => createPlay(s.priority))
      case Finesse if priorityToBeat > Rank.honorPriority =>
        choosePlays(deal, strain, hand, WinIt, currentWinner)
      case WinIt if isPartnerWinning =>
        chooseFollowSuitPlays(createPlay, Duck, priorityToBeat)
      case _ =>
        chooseFollowSuitPlays(createPlay, strategy, priorityToBeat)
    }
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
    * Adjust the priorities of this Holding by considering partner as cooperative.
    *
    * @param holding the cooperative holding.
    * @return an eagerly promoted X.
    */
  def cooperate(holding: Holding): Holding = _cooperate(holding)


  def reprioritize: Holding = _reprioritize

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

  private lazy val _quit = {

    def applyPromotions(sequence: Sequence): Sequence = {
      val promotion = promotions.count(_ < sequence.priority)
      Sequence(sequence.priority - promotion, sequence.cards)
    }

    val ss: List[Sequence] = sequences map applyPromotions
    Holding(ss.foldLeft[List[Sequence]](Nil)((r, s) => s.merge(r)), suit, Nil)
  }

  private def _cooperate(holding: Holding) = Holding(sequences, suit, for (s <- holding.sequences; p = s.priority; is <- List.fill(s.length)(p)) yield is)

  private lazy val _reprioritize: Holding = Holding(for (s <- sequences) yield s.reprioritize, suit, promotions)

  private lazy val hasHonorSequence: Boolean = realSequences exists (_.isHonor)

  private lazy val realSequences = sequences filter (_.cards.lengthCompare(1) > 0)

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

  // TODO Merge this with the following method
  private def chooseFollowSuitPlays(createPlay: Int => CardPlay, strategy: Strategy, priorityToBeat: Int): List[CardPlay] = {
    // XXX this function is used to sort the possible plays according to which fits the given strategy best (smallest resulting Int)
    def sortFunction(play: CardPlay): Int = Holding.applyFollowSuitStrategy(strategy, priorityToBeat, play.priority)

    (for (s <- sequences) yield createPlay(s.priority)).sortBy(sortFunction)
  }

  private def chooseLeadSuitPlays(createPlay: Int => CardPlay, strategy: Strategy): List[CardPlay] = {
    // XXX this function is used to sort the possible plays according to which fits the given strategy best (smallest resulting Int)
    def sortFunction(play: CardPlay): Int = Holding.applyLeadSuitStrategy(strategy, play, sequence(play.priority))

    (for (s <- sequences) yield createPlay(s.priority)).sortBy(sortFunction)
  }

  private def getStrategyForFollowingSuit(trick: Trick): Strategy = trick.size match {
    // XXX this first case should never occur.
    case 0 => if (hasHonorSequence) LeadTopOfSequence else FourthBest
    case 1 => if (trick.isHonorLed || realSequences.nonEmpty) Cover else Duck
    case 2 => Finesse // XXX becomes WinIt if card to beat isn't an honor
    case 3 => Cover
    case x => throw CardException(s"too many prior plays: $x")
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
    val cards = ranks map (rank => Card(suit, rank)) to List
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
  def create(suit: Suit, cards: List[Card]): Holding = apply(suit, (cards map (_.rank)).sorted.reverse: _*)

  def create(ranks: List[Rank], suit: Suit): Holding = apply(suit, ranks.sorted.reverse: _*)

  def ranksToString(ranks: List[Rank]): String = if (ranks.nonEmpty) ranks.mkString("", "", "") else "-"
  //
  //  // NOTE not used
  //  implicit object LoggableHolding extends Loggable[Holding] with Loggables {
  //    def toLog(t: Holding): String = t.neatOutput
  //  }

  /**
    * Method to assess the given strategy in the current situation.
    * CHECK: Can include opening lead situations ??
    *
    * @param strategy      the required strategy.
    * @param currentWinner the priority of the card play which is currently winning this trick.
    * @param priority      the priority of the card (sequence) being considered.
    * @return a relatively low number (e.g. 0) if this matches the given strategy, otherwise a high number.
    */
  //	private
  def applyFollowSuitStrategy(strategy: Strategy, currentWinner: Int, priority: Int): Int = {
    lazy val rank = 2 * Rank.lowestPriority - priority // XXX the rank of the played card plus 14

    lazy val applyPotentialWinStrategy =
      if (strategy.conditional)
        currentWinner - priority // XXX prefer the card that wins by the slimmest margin (always positive)
      else if (strategy.win)
        priority // XXX play high.
      else
        rank // XXX play low.

    if (priority < currentWinner) // XXX can we win this trick if we want to?
      applyPotentialWinStrategy
    else
      rank // XXX play low.
  }

  /**
    * Method to assess the given strategy in the current situation.
    * CHECK: Can include opening lead situations ??
    *
    * @param strategy the required strategy.
    * @return a relatively low number (e.g. 0) if this matches the given strategy, otherwise a high number.
    */
  //	private
  def applyLeadSuitStrategy(strategy: Strategy, play: CardPlay, maybeSequence: Option[Sequence]): Int = {
    strategy match {
      case LeadTopOfSequence => play.priority

      // TODO need to implement this properly
      case LeadSecond => play.priority

      case FourthBest => Rank.lowestPriority - play.priority

      case Stiff => maybeSequence.map(s => s.priority).getOrElse(14)

      case _ => 10
    }
  }
}
