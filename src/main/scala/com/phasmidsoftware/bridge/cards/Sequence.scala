/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import scala.language.implicitConversions

/**
  * A (non-empty) sequence of cards forming part of a Holding.
  * In practice, partners "cooperate" with each other such that they mutually form sequences.
  * This reduces the total number of sequences in a deal, which speeds up processing.
  *
  * @param priority the number of higher-ranking cards in the suit.
  * @param cards    the cards.
  */
case class Sequence(priority: Int, cards: List[Card]) extends Evaluatable with Reprioritizable[Sequence] {

  require(cards.nonEmpty)

  /**
    * @return the length of this sequence times one-half to the power of priority.
    */
  def evaluate: Double = _evaluate

  /**
    *
    * @return a Sequence based on the cards, ignoring the current priority.
    */
  def reprioritize: Sequence = Sequence(cards)

  /**
    * @return a String primarily for debugging purposes.
    */
  override def toString: String = s"${cards.map(_.rank).mkString("")}[$priority]"

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
    * TEST me
    *
    * @return true if the top card of the sequence indicated is at least a ten.
    */
  lazy val isHonor: Boolean = Sequence.isHonor(priority)

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
  def apply(cs: List[Card]): Sequence = apply(cs.head.priority, cs)

  /**
    * @return true if the top card of the sequence with the given priority is at least a ten.
    */
  def isHonor(priority: Int): Boolean = priority <= Rank.honorPriority

  /**
    * An ordering for a Sequence.
    * Lower values of priority precede higher values.
    */
  implicit object SequenceOrdering extends Ordering[Sequence] {
    override def compare(x: Sequence, y: Sequence): Int = x.priority - y.priority
  }
  //
  //  /**
  //    * An loggable for a Sequence.
  //    * Lower values of priority precede higher values.
  //    *
  //    * NOTE: not used
  //    */
  //  implicit object LoggableSequence extends Loggable[Sequence] with Loggables {
  //    implicit val cardSequenceLoggable: Loggable[List[Card]] = listLoggable[Card]
  //    // NOTE: that, for this particular apply method, we have to specify the fields we need.
  //    val loggableSequence: Loggable[Sequence] = toLog2(Sequence.apply, List("priority", "cards"))
  //
  //    def toLog(t: Sequence): String = loggableSequence.toLog(t)
  //  }

}
