/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.gambit.util.{Output, Outputable}

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
case class Holding(sequences: Seq[Sequence], suit: Suit, promotions: Seq[Int] = Nil)
  extends Outputable[Unit] with Quittable[Holding] with Cooperative[Holding] with Reprioritizable[Holding] with Evaluatable with Removable {

  require(isVoid || maybeSuit.get == suit)

  /**
    * @return the number of sequences in this Holding
    */
  lazy val size: Int = sequences.size

  /**
    * @return the number of cards in this Holding (i.e., the suit length)
    */
  lazy val length: Int =
    sequences.map(_.length).sum

  /**
    * Optionally yield a Sequence that matches the given priority.
    *
    * @param priority the priority to be matched.
    * @return an Option[Sequence].
    */
  def sequence(priority: Int): Option[Sequence] =
    sequences.find(s => s.priority == priority)

  /**
    * @return the all of the cards in this Holding.
    */
  lazy val cards: Seq[Card] =
    for (s <- sequences; c <- s.cards) yield c

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
    * @param trick the current state of this trick (i.e., the prior plays).
    * @return a sequence of all possible plays, starting with the ones most suited to the appropriate strategy.
    */
  def chooseFollowSuitPlays(deal: Deal, strain: Option[Suit], hand: Int, trick: Trick): Seq[CardPlay] =
    choosePlays(deal, strain, hand, getStrategyForFollowingSuit(trick), trick.winner)

  /**
    * For now, we ignore strategy which is only used to ensure that we try the likely more successful card play first.
    *
    * @param deal          the deal to which these plays will belong.
    * @param hand          the index of this Hand (N, E, S, W).
    * @param strain        the (optional) trump suit.
    * @param strategy      the recommended strategy.
    * @param currentWinner the play currently winning the trick.
    * @return a sequence of `CardPlay` objects.
    */
  def choosePlays(deal: Deal, strain: Option[Suit], hand: Int, strategy: Strategy, currentWinner: Option[Winner]): Seq[CardPlay] = {
    def createPlay(priority: Int): CardPlay = CardPlay(deal, strain, hand, suit, priority)

    lazy val priorityToBeat = currentWinner.map(_.priorityToBeat(hand)).getOrElse(Rank.lowestPriority)
    lazy val isPartnerWinning = currentWinner.exists(_.partnerIsWinning(hand))

    def redirect(s: Strategy) = choosePlays(deal, strain, hand, s, currentWinner)

    strategy match {
      case StandardOpeningLead if hasHonorSequence =>
        redirect(LeadTopOfSequence)
      case StandardOpeningLead =>
        sortLeadSuitPlays(createPlay, strategy)
      case Ruff if isPartnerWinning =>
        redirect(Discard)
      case Ruff | Discard =>
        sequences.lastOption.toList.map(s => createPlay(s.priority))
      case Finesse if priorityToBeat > Rank.honorPriority =>
        redirect(WinIt)
      case WinIt if isPartnerWinning =>
        sortFollowSuitPlays(createPlay, Duck, priorityToBeat)
      case _ =>
        sortFollowSuitPlays(createPlay, strategy, priorityToBeat)
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
  def promote(priority: Int): Holding =
    Holding(sequences, suit, promotions :+ priority)

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
    * Method to remove (i.e., play) a card from this Holding.
    *
    * @param priority the sequence from which the card will be played.
    * @return a new Holding with the sequence either truncated or eliminated entirely.
    */
  //noinspection ScalaStyle
  def -(priority: Int): Holding = {
    val sos: Seq[Option[Sequence]] = for (s <- sequences) yield if (s.priority == priority) s.truncate else Some(s)
    Holding(sos.flatten, suit, promotions)
  }

  /**
    * Determines if the priorities of all sequences in this Holding
    * align with the sequences of the given partner Holding according to
    * specific adjustment conditions.
    *
    * @param partner the partner Holding to compare with for adjustments.
    * @return true if the sequences in this Holding align with the partner Holding
    *         based on the adjustment rules; false otherwise.
    */
  def isAdjustedWith(partner: Holding): Boolean =
    partner.sequences.forall { ps =>
      ps.cards.forall { card =>
        val adjacentSeqs = sequences.filter { ms =>
          ms.priority + ms.length == card.priority ||
            card.priority + 1 == ms.priority
        }
        adjacentSeqs.size <= 1
      }
    }

  /**
    * @return a String primarily for debugging purposes.
    */
  override def toString: String =
    s"{$suit: ${sequences.mkString(", ")}} " + (if (promotions.nonEmpty) promotions.mkString(", ") else "(clean)")

  /**
    * @return a neat representation of this Sequence.
    */
  lazy val neatOutput: String =
    s"$suit${Holding.ranksToString(cards map (_.rank))}"

  /**
    * @return a neat representation of this Sequence (without suit symbol).
    */
  lazy val asPBN: String =
    s"${Holding.ranksToString(cards map (_.rank))}"

  /**
    * Output this Sequence to an Output.
    *
    * @param output the output to append to.
    * @param xo     an optional value of Unit, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output =
    output :+ suit.toString :+ Holding.ranksToString(cards map (_.rank))

  /**
    * A private lazy value that enacts pending promotions for `Sequence` objects and creates
    * a new `Holding` instance with the adjusted sequences.
    * This value is computed only when accessed, following a lazy initialization strategy.
    *
    * Inside, the method `applyPromotions` is defined to adjust the priority of a `Sequence`
    * based on the number of pending promotions with lower priorities than the sequence.
    *
    * Steps:
    * 1. Promotes each `Sequence` in the `sequences` collection by decrementing its priority
    *    according to the count of applicable promotions.
    * 2. Combines these promoted `Sequence` objects into a final collection using the `merge` method.
    * 3. Wraps the resulting collection into a new `Holding` object, while maintaining
    *    the characteristics of the original `Holding`.
    */
  private lazy val _quit = {

    def applyPromotions(sequence: Sequence): Sequence = {
      val promotion = promotions.count(_ < sequence.priority)
      Sequence(sequence.priority - promotion, sequence.cards)
    }

    val ss: Seq[Sequence] = sequences map applyPromotions
    Holding(ss.foldLeft[Seq[Sequence]](Nil)((r, s) => s.merge(r)), suit, Nil)
  }

  /**
    * Adjusts the priorities of this `Holding` by considering the given holding as cooperative.
    *
    * TODO (Important) this method is invoked for performance optimization
    * (it potentially reduces the number of possible plays by considering the combined sequence where appropriate).
    * However, if this `Holding` or partner's `Holding` actually wins the trick,
    * we must go back and consider the unadjusted priorities because the lead to the next trick
    * depends on the actual winner.
    *
    * @param holding the cooperative holding that influences the priorities.
    * @return a new `Holding` with adjusted priorities based on cooperation.
    */
  private def _cooperate(holding: Holding) =
    Holding(sequences, suit, for (s <- holding.sequences; p = s.priority; is <- List.fill(s.length)(p)) yield is)

  /**
    * A lazily initialized instance of Holding that adjusts the priorities of the sequences
    * in this Holding by invoking the `reprioritize` function on each sequence.
    *
    * Each sequence in `sequences` undergoes reprioritization, and the results are used
    * to create a new Holding with the same suit and promotions as the current one.
    *
    * This value is computed and cached when accessed for the first time, and the computation
    * will not be repeated on subsequent accesses.
    *
    * @return a new Holding with reprioritized sequences while maintaining the same suit
    *         and promotions of the original Holding.
    */
  private lazy val _reprioritize: Holding =
    Holding(for (s <- sequences) yield s.reprioritize, suit, promotions)

  /**
    * Determines if this Holding contains at least one sequence that is classified as an honor sequence.
    *
    * A sequence is considered an honor sequence if it satisfies the `isHonor` property.
    * This value is computed lazily and checks all sequences in this Holding's `realSequences` collection.
    *
    * @return true if at least one sequence is an honor sequence, false otherwise.
    */
  private lazy val hasHonorSequence: Boolean =
    realSequences exists (_.isHonor)

  /**
    * Filters the sequences within this holding to retain only those with more than one card.
    *
    * This lazy value evaluates the `sequences` collection and applies a filter to include
    * only the sequences where the number of cards exceeds one. The filtering operation is
    * deferred until the value is accessed.
    */
  private lazy val realSequences =
    sequences filter (_.cards.lengthCompare(1) > 0)

  /**
    * Lazily retrieves the Suit of the first card in the Holding, if it exists.
    *
    * This method examines the `cards` collection in the current Holding and retrieves the Suit of the first card
    * using its `suit` property. If the collection of `cards` is empty, the result will be `None`.
    *
    * @return an optional `Suit`, which will be `Some(suit)` if there is at least one card in the Holding, or `None` if the Holding is empty.
    */
  private lazy val maybeSuit: Option[Suit] =
    cards.headOption map (_.suit)

  /**
    * Lazily evaluates a Double value based on the evaluations of a collection of sequences.
    * The calculation is performed using iteration, where each sequence's evaluation is
    * weighted by powers of 2 based on cumulative card counts.
    *
    * The evaluation involves iterating through all sequences, calculating the evaluation
    * of each sequence, and accumulating a weighted result. The weight for each sequence
    * increases exponentially with the number of cards encountered so far.
    *
    * Note: This method depends on the `sequences` collection and assumes that each sequence
    * has an `evaluate` method and a `length` attribute.
    *
    * @return the evaluated Double value of the sequences with exponential weighting.
    */
  private lazy val _evaluate: Double = {
    // CONSIDER Do this properly but, for now, I'm going to use iteration and var !!
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

  /**
    * Sorts potential follow-suit plays based on how well they align with the given strategy and their ability to beat the specified priority.
    *
    * This method generates possible card plays using the provided `createPlay` function, ranks them according to the specified
    * strategy, and returns the plays in order of their suitability to the strategy as determined by a heuristic scoring function.
    *
    * @param createPlay     a function that creates a `CardPlay` instance from a given priority value.
    * @param strategy       the `Strategy` guiding the evaluation and sorting of plays.
    * @param priorityToBeat the priority value that the play needs to beat or consider during the sorting process.
    * @return a sequence of `CardPlay` objects, ordered by their alignment with the strategy and their effectiveness in relation to the priority to beat.
    */
  private def sortFollowSuitPlays(createPlay: Int => CardPlay, strategy: Strategy, priorityToBeat: Int): Seq[CardPlay] = {
    def sortFunction(play: CardPlay): Int =
      Holding.applyFollowSuitStrategy(strategy, priorityToBeat, play.priority)

    (for (s <- sequences) yield createPlay(s.priority)).sortBy(sortFunction)
  }

  /**
    * Sorts potential card plays based on how well they align with the given strategy.
    *
    * This method generates possible card plays using the provided `createPlay` function,
    * ranks them according to the specified strategy, and returns the plays in order
    * of their suitability to the strategy as determined by a heuristic scoring function.
    *
    * @param createPlay a function that creates a `CardPlay` instance from a given priority value.
    * @param strategy   the `Strategy` guiding the evaluation and sorting of plays.
    * @return a sequence of `CardPlay` objects, ordered by their alignment with the strategy.
    */
  private def sortLeadSuitPlays(createPlay: Int => CardPlay, strategy: Strategy): Seq[CardPlay] = {
    def sortFunction(play: CardPlay): Int =
      Holding.applyLeadSuitStrategy(strategy, play, sequence(play.priority))

    (for (s <- sequences) yield createPlay(s.priority)).sortBy(sortFunction)
  }

  /**
    * Determines the appropriate strategy for following suit in a trick based on the current state of the trick.
    *
    * @param trick the current state of the trick, including the cards played so far.
    * @return a Strategy object representing the appropriate course of action for following suit.
    *         Possible strategies include:
    *         - LeadTopOfSequence: If no cards have been played and there is an honor sequence.
    *         - FourthBest: If no cards have been played and there is no honor sequence.
    *         - Cover: If one card has been played, and it is an honor or there are real sequences.
    *         - Duck: If one card has been played and conditions for Cover are not met.
    *         - Finesse: If two cards have been played.
    *         - WinIt: A speculative refinement for Finesse when the card to beat isn't an honor.
    *         - Cover: If three cards have been played.
    * @throws CardException if the trick contains more than three prior plays (invalid state).
    */
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
  def apply(suit: Suit, ranks: String): Holding =
    create(Card.parser.parseRanks(ranks).toList, suit)

  /**
    * Implicit converter from a String to a Holding.
    *
    * @param s the String made up of (abbreviated) suit and ranks.
    * @return a new Holding.
    */
  implicit def parseHolding(s: String): Holding =
    create(Card.parser.parseRanks(s.tail).toList, Suit(s.head))

  /**
    * An ordering for Ranks.
    * Lower priorities precede higher priorities.
    * CONSIDER merge with duplicate code.
    */
  implicit object RankOrdering extends Ordering[Rank] {
    override def compare(x: Rank, y: Rank): Int =
      -x.priority + y.priority
  }

  /**
    * Creates a Holding from the given suit and a sequence of cards.
    * CONSIDER merging the two `create` methods
    *
    * @param suit  the suit to associate with the Holding.
    * @param cards the sequence of cards used to construct the Holding.
    * @return a new Holding containing the sorted ranks of the provided cards in the specified suit.
    */
  def create(suit: Suit, cards: Seq[Card]): Holding =
    apply(suit, (cards map (_.rank)).sorted.reverse *)

  /**
    * Creates a new Holding instance using a sequence of ranks and a suit.
    * The ranks will be sorted in descending order before creating the Holding.
    *
    * @param ranks the sequence of ranks to include in the Holding
    * @param suit  the suit associated with the Holding
    * @return a new Holding instance
    */
  def create(ranks: Seq[Rank], suit: Suit): Holding =
    apply(suit, ranks.sorted.reverse *)

  /**
    * Method to assess the given strategy in the current situation.
    * CHECK: Can include opening lead situations ??
    *
    * @param strategy      the required strategy.
    * @param currentWinner the priority of the card play that is currently winning this trick.
    * @param priority      the priority of the card (sequence) being considered.
    * @return a relatively low number (e.g., 0) if this matches the given strategy, otherwise a high number.
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
    * @return a relatively low number (e.g., 0) if this matches the given strategy, otherwise a high number.
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

  private def ranksToString(ranks: Seq[Rank]): String = if (ranks.nonEmpty) ranks.mkString("", "", "") else "-"
}
