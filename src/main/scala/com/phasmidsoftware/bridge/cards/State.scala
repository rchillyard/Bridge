/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.gambit.util.{Output, Outputable}

import scala.language.postfixOps

/**
  * Case class State to represent a possible state of play in analysis of a Deal.
  * The state must be consistent, which is to say that the deal is the result of playing all of the previous tricks AND
  * the current (possibly partial) trick.
  *
  * @param whist  the whist game that is being played.
  * @param trick  the Trick.
  * @param tricks the current trick totals.
  */
case class State(whist: Whist, trick: Trick, tricks: Tricks) extends Outputable[Unit] with Validatable:

  val sequence: Int = State.getSequence

  def evaluateKey: (Long, Long, Long, Long) = {
    def handBits(hand: Hand): Long =
      hand.cards.foldLeft(0L)((acc, c) => acc | (1L << c.cardIndex))

    val h: Seq[Hand] = whist.deal.hands
    (handBits(h.head), handBits(h(1)), handBits(h(2)), handBits(h(3)))
  }
    
  /**
    * Method to enumerate all of the possible states that could be children of this State.
    *
    * @return a sequence of States.
    */
  def enumeratePlays: Seq[State] = _enumeratePlays

  /**
    * NOTE: used only in unit tests.
    * Method to get the next State in sequence.
    *
    * @param trick the next Trick.
    * @return a new consistent State based on deal and trick.
    */
  def next(trick: Trick): State = State.create(whist, trick, tricks)

  /**
    * NOTE: used only in unit tests.
    *
    * @return true if the number of cards played plus the number of cards remaining equals 52.
    */
  def isConsistent: Boolean = _isConsistent

  /**
    * NOTE: used only in unit tests.
    * Method to validate this State.
    *
    * @return true if all the plays of the trick are validated.
    */
  def validate: Boolean = _validate

  /** The deal referenced by this State. */
  val deal: Deal = whist.deal

  /**
    * The total number of cards played at this state of the game.
    *
    * @return an Int 0..52
    */
  lazy val cardsPlayed: Int = trick.cardsPlayed

  /**
    * The heuristic fitness of this State: tricks taken by NS plus NS trick-taking potential.
    * Rounded to the nearest 0.1.
    */
  lazy val fitness: Double = math.rint(State.heuristicFitness(this) * 10) / 10

  /**
    * Method to yield neat output for a State.
    *
    * @return a compact String
    */
  def neatOutput: String =
    s"""State: Trick History: "${trick.history.mkString("", ", ", "")}" $tricks $fitness ${deal.neatOutput}"""

  /**
    * Invokes output on the trick, passing it Some(deal) and appending the fitness in parentheses.
    *
    * @param output the output to append to.
    * @param xo     an optional value of X, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output =
    trick.output(output, Some(deal)) :+ s" ($fitness)"

  private lazy val _enumeratePlays = whist.makeStates(tricks, trick.enumerateSubsequentPlays(whist))
  private lazy val _validate: Boolean = trick.plays.forall(_.validate)
  private lazy val _isConsistent = trick.cardsPlayed + deal.nCards == 52

object State:

  given loggableState: Loggable[State] = s => s.neatOutput

  var count: Int = 0

  def getSequence: Int = { count = count + 1; count }

  /**
    * Evaluate the heuristic fitness of a State.
    * We add the number of tricks owned by NS to the potential of NS to take more tricks.
    * EW tricks already taken are ignored.
    *
    * @param s the State to evaluate.
    * @return the fitness as a Double.
    */
  def heuristicFitness(s: State): Double =
    s.tricks.ns + s.deal.evaluate

  /**
    * Method to create an initial state based on a deal.
    *
    * @param whist the game we are playing.
    * @param trick the current trick.
    * @return a new State based on the game.
    */
  def apply(whist: Whist, trick: Trick): State = apply(whist, trick, Tricks.zero.increment(trick))

  /**
    * Method to create an initial state based on a Whist game.
    *
    * @param whist the game we are playing.
    * @return a new State with an empty trick.
    */
  def apply(whist: Whist): State = apply(whist, Trick.empty)

  /**
    * Method to create a new State based on the outcome of the current trick.
    * If the trick is complete, creates a new State with an updated Whist and incremented tricks.
    * If the trick is incomplete, creates a new State with an updated Whist and unchanged tricks.
    *
    * @param whist       the current state of the game.
    * @param trick  the current trick.
    * @param tricks the current state of the tricks.
    * @return a new State.
    */
  def create(whist: Whist, trick: Trick, tricks: Tricks): State =
    if trick.started then
      if trick.isComplete then State(whist.play(trick.plays.last).quit, trick, tricks.increment(trick))
      else State(whist.play(trick.plays.last), trick, tricks)
    else throw CardException(s"cannot create a new State based on an empty trick")

  given StateOrdering: Ordering[State] with
    /**
      * Compare two States by number of cards played (more played = later in sequence).
      */
    def compare(x: State, y: State): Int =
      y.cardsPlayed - x.cardsPlayed

/**
  * Behavior of something which can be validated.
  */
trait Validatable:
  def validate: Boolean