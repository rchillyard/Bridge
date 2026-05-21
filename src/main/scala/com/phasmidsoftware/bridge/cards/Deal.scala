/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.cards.Rank.ranks
import com.phasmidsoftware.bridge.cards.Suit.suits
import com.phasmidsoftware.gambit.util.{Output, Outputable, Shuffle}

import java.io.Writer
import scala.language.postfixOps

/**
  * Class to describe a Deal, that's to say a particular arrangement of cards distributed to four separate holdings.
  * In particular, this class describes the current state of a Deal, so there will not always be 52 cards in it.
  * The Holdings are typically lazily promoted--for an eagerly promoted Deal, you should invoke quit.
  *
  * The constructor does NOT adjust for partnerships because that should only be performed at the start of
  * the life of a Deal.
  *
  * @param title    the title of this Deal.
  * @param holdings the holdings of the four Hands of this Deal.
  */
case class Deal(title: String, holdings: Map[Int, Map[Suit, Holding]]) extends Outputable[Unit]
  with Quittable[Deal] with Playable[Deal] with Evaluatable with Validatable {

  /**
    * Method to yield the Suit/Holding of the partner of a particular holding.
    *
    * @param k the index of the holding whose partner we need.
    * @return a Map of Suit, Holding pairs.
    */
  def partner(k: Int): Map[Suit, Holding] = holdings((k + 2) % Deal.HandsPerDeal)

  /**
    * Method to return a sequence representing the four hands of this Deal.
    */
  val hands: Seq[Hand] = (for ((i, hs) <- holdings) yield Hand(i, hs)).toSeq

  /**
    * @return an eagerly promoted Deal.
    */
  def quit: Deal = _quit

  /**
    * Asserts that the deal has been adjusted and that the adjustment invariant holds.
    * This method verifies the internal consistency of the deal after an adjustment
    * by asserting that either the deal is not adjusted, or it satisfies the adjustment invariant.
    *
    * @return Unit (no value is returned, but an assertion error is thrown if the invariant is violated)
    */
  def assertAdjusted(): Unit =
    assert(checkAdjustmentInvariant, "adjustment invariant violated")

  /**
    * Apply adjustments based on cooperation from partner.
    *
    * @return an eagerly promoted X.
    */
  lazy val adjustForPartnerships: Deal =
    println(s"Adjusting deal for partnerships: $title")
    val result = _cooperate._quit._reprioritize
    result.assertAdjusted()
    result

  /**
    * Play a card from this Deal.
    *
    * @param cardPlay the card play.
    * @return a new Playable.
    */
  def play(cardPlay: CardPlay): Deal =
    Deal(title, hands map (_.play(cardPlay)))

  /**
    * Evaluate the N and S hands heuristically.
    *
    * @return a number that corresponds to the trick-taking ability of the N/S hands.
    */
  lazy val evaluate: Double = evaluateNS - evaluateEW
  lazy val evaluateNS: Double = hands.head.evaluate + hands(2).evaluate
  lazy val evaluateEW: Double = hands(1).evaluate + hands(3).evaluate
  /**
    * Method to check this Deal.
    *
    * @return true if this Deal is valid.
    */
  def validate: Boolean = hands.forall(_.validate) && allCards

  /**
    * @return the number of cards remaining in this Deal.
    */
  lazy val nCards: Int = hands map (_.nCards) sum

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
  lazy val neatOutput: String = s"Deal $title ($nCards) ${hands.map(_.neatOutput)}"

  /**
    * @return a String that represents this Deal, primarily for debugging purposes.
    */
  override def toString: String =
    s"Deal $title ($nCards cards and $countSequences sequences)"

  /**
    * Converts the provided metadata map and board number into a PBN (Portable Bridge Notation) representation.
    * The method constructs a PBN string by combining metadata, board information, and deal details.
    *
    * @param map   a map containing key-value pairs of metadata to include in the PBN representation
    * @param board an integer representing the board number
    * @return a string representing the PBN format for the given metadata and board information
    */
  def asPBN(map: Map[String, String], board: Int): String = {
    val result = new StringBuilder()
    for ((k, v) <- map) result.append(s"""[$k "$v"]\n""")
    result.append(s"""[Board "$board"]\n""")
    result.append("""[Deal "N:""")
    result.append(hands.map(_.asPBN).mkString("", " ", ""))
    result.append(""""]""" + "\n")
    result.toString
  }

  /**
    * Computes the total count of sequences across all holdings within this deal.
    *
    * A sequence refers to a specific property of a holding, and the computation aggregates
    * the length of sequences from all holdings across the available hands in the deal.
    *
    * The result is obtained using nested "for" comprehensions:
    * - Outer loop iterates through the holdings.
    * - Inner loop iterates through the mappings of suits to holdings.
    * - For each mapping, it extracts the length of sequences and accumulates the total.
    */
  lazy val countSequences: Int =
    (for ((_, m) <- holdings; (_, h) <- m; q = h.sequences.length) yield q).sum

  private def checkAdjustmentInvariant: Boolean =
    (0 until Deal.HandsPerDeal by 2).forall { k =>
      Suit.suits.forall { suit =>
        val myHolding = holdings(k).getOrElse(suit, Holding(suit))
        val partnerHolding = holdings((k + 2) % Deal.HandsPerDeal).getOrElse(suit, Holding(suit))
        myHolding.isAdjustedWith(partnerHolding)
      }
    }

  private[cards] lazy val north: Hand = n

  private[cards] lazy val east: Hand = e

  private[cards] lazy val south: Hand = s

  private[cards] lazy val west: Hand = w

  /**
    * Play a trick (made up of four card plays).
    *
    * NOTE: only invoked by unit tests.
    *
    * @param trick the card play from each of the four hands.
    */
  private[cards] def playAll(trick: Trick): Deal =
    Deal(title, hands map (_.playAll(trick)))

  private val Seq(n, e, s, w) = hands

  private lazy val _quit =
    Deal(title, for ((k, v) <- holdings) yield k -> (for ((s, h) <- v) yield s -> h.quit))

  /**
    * CONSIDER Should be private
    */
  lazy val _cooperate: Deal =
    Deal(title, for ((k, v) <- holdings) yield k -> (for ((s, h) <- v) yield s -> h.cooperate(partner(k).getOrElse(s, Holding(s)))))

  lazy val _reprioritize: Deal =
    Deal(title, for ((k, v) <- holdings) yield k -> (for ((s, h) <- v) yield s -> h.reprioritize))

  private def outputHand(name: String, hand: Hand): Output =
    (Output(s"$name:\t") :+ hand.neatOutput).insertBreak

  //  private lazy val _evaluate =
  //    hands.sliding(1, 2).flatten.map(_.evaluate).sum

  private lazy val allCards: Boolean =
    hands.flatMap(_.cards).distinct.size == Deal.CardsPerDeal

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

  def apply(title: String, holdings: Map[Int, Map[Suit, Holding]]): Deal = new Deal(title, holdings)

  /**
    * Create a new Deal from a title and four Hands.
    *
    * @param title the title.
    * @param hands the four Hands.
    * @return a new Deal.
    */
  def apply(title: String, hands: Seq[Hand]): Deal = apply(title, (for (h <- hands) yield h.index -> h.holdings).toMap)

  /**
    * Construct a Deal from a random number generator which will yield an arrangement of cards.
    * This method does NOT adjust for partnerships as it is used principally for testing.
    *
    * @param title                 a title for the Deal.
    * @param seed                  a seed for the random number generator (defaults to the system--nano--clock)
    * @param adjustForPartnerships (defaults to true) if true then the result will have priorities adjusted for partnerships.
    * @return a new Deal.
    */
  def createRandom(title: String, seed: Long = System.nanoTime(), adjustForPartnerships: Boolean = true): Deal = {
    val newDeck: Seq[Card] =
      for (s <- suits; r <- ranks) yield Card(s, r)
    val shuffler: Iterable[Card] => Seq[Card] = Shuffle[Card](_, seed)
    fromCards(title, shuffler(newDeck), adjustForPartnerships)
  }

  /**
    * Construct a Deal from a sequence of Cards.
    * Adjusts for Partnerships.
    *
    * NOTE: it appears that this method is only ever used by unit tests.
    *
    * @param title  a title for the new Deal.
    * @param cs     the cards dealt in sequence.
    * @param adjust adjustForPartnerships if true (default)
    * @return a new Deal.
    */
  def fromCards(title: String, cs: Seq[Card], adjust: Boolean = true): Deal = {
    val deal = new Deal(title, (for ((cs, index) <- cs.grouped(CardsPerHand).zipWithIndex) yield index -> Hand.createHoldings(cs)).toMap)
    if (adjust)
      deal.adjustForPartnerships
    else
      deal
  }

  /**
    * Construct a Deal of sequence of a sequence of Card representations.
    *
    * @param title the title for the deal.
    * @param start the player who will lead to this deal.
    * @param wss   a sequence in order NESW of a sequence in order SHDC of card representations.
    * @return a new deal. Yeah to FDR.
    */
  def fromHandStrings(title: String, start: String, wss: Seq[Seq[String]]): Deal = {
    val firstIndex = start match {
      case "N" => 0;
      case "E" => 1;
      case "S" => 2;
      case "W" => 3
    }
    val nCards = wss.flatten.map(_.length).sum
    if (nCards % Deal.CardsPerTrick != 0) throw new IllegalArgumentException(s"Number of cards must be a multiple of $CardsPerTrick, but got $nCards")
    val hSss: Seq[Seq[(Suit, Holding)]] = for (ws <- wss) yield for ((w, x) <- ws zip suits) yield x -> Holding(x, w)
    val hands = for ((hHs, i) <- hSss zipWithIndex) yield Hand(Hand.next(firstIndex, i), hHs.toMap)
    val cardsPerHand = nCards / Deal.CardsPerTrick
    val cards = for {
      hand <- hands
      if hand.cards.size == cardsPerHand
      (suit, holding) <- hand.holdings
      sequence <- holding.sequences
      card <- sequence.cards
    } yield card
    val set = Set.from(cards)
    if (set.size != nCards) throw new IllegalStateException(s"Expected $nCards unique cards, but got ${set.size}")
    val (nonNorth, north) = hands.splitAt(4 - firstIndex)
    Deal(title, north ++ nonNorth).adjustForPartnerships
  }

  def writePBN(writer: Writer, map: Map[String, String], boards: Seq[Deal]): Unit = {
    writer.append("% PBN 2.1\n% EXPORT\n")
    for ((d, i) <- boards.zipWithIndex) writer.append(d.asPBN(map, i + 1))
    writer.flush()
  }

  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)
}
