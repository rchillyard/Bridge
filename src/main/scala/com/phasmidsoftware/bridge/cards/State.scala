/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.decisiontree.Fitness
import com.phasmidsoftware.util._

import scala.language.postfixOps

/**
  * Case class State to represent a possible state of play in analysis of a Deal.
  * The state must be consistent, which is to say that the deal is the result of playing all of the previous tricks AND
  * the current (possibly partial) trick.
  *
  * @param whist the whist game that is being played.
  * @param trick the Trick.
  */
case class State(whist: Whist, trick: Trick, tricks: Tricks) extends Outputable[Unit] with Validatable {

  val sequence: Int = State.getSequence

  /**
    * Method to enumerate all of the possible states that could be children of the Node enclosing this State.
    *
    * @return a sequence of States.
    */
  def enumeratePlays: List[State] = _enumeratePlays

  /**
    * NOTE: this is used only in unit tests
    *
    * Method to get the next State in sequence.
    *
    * @param trick the next Trick.
    * @return a new consistent State based on deal and t.
    */
  def next(trick: Trick): State = State.create(whist, trick, tricks)

  /**
    * NOTE: this is used only in unit tests
    *
    * @return true if the number of cards played according to the trick plus the number of cares remaining in the deal equals 52
    */
  def isConsistent: Boolean = _isConsistent

  /**
    * NOTE: this is used only in unit tests
    *
    * Method to validate this State.
    *
    * TODO test this.
    *
    * @return true if all the plays of the trick are validated
    */
  def validate: Boolean = _validate

  /**
    * The deal referenced by this State.
    */
  val deal: Deal = whist.deal

  /**
    * The total number of cards played at this state of the game.
    *
    * @return an Int 0..52
    */
  lazy val cardsPlayed: Int = trick.cardsPlayed

  /**
    * @return the fitness of this State rounded to the nearest 0.1
    */
  lazy val fitness: Double = math.rint(State.StateFitness.fitness(this) * 10) / 10

  /**
    * Method to yield neat output for a State.
    *
    * @return a compact String
    */
  def neatOutput: String = s"""State: Trick History: "${trick.history.mkString("", ", ", "")}" $tricks $fitness ${deal.neatOutput}"""

  /**
    * Invokes output on the trick, passing it Some(deal) and appending the fitness in parentheses.
    *
    * @param output the output to append to.
    * @param xo     an optional value of X, defaulting to None.
    * @return a new instance of Output.
    */
  def output(output: Output, xo: Option[Unit] = None): Output = trick.output(output, Some(deal)) :+ s" ($fitness)"

  private lazy val _enumeratePlays = whist.makeStates(tricks, trick.enumerateSubsequentPlays(whist))

  private lazy val _validate: Boolean = trick.plays.forall(_.validate)

  private lazy val _isConsistent = trick.cardsPlayed + deal.nCards == 52 // && validate
}


object State {
  var count: Int = 0

  def getSequence: Int = {
    count = count + 1; count
  }

  /**
    * Method to create an initial state based on a deal.
    *
    * @param whist the game we are playing.
    * @return a new State based on the game, without any tricks having been played.
    */
  def apply(whist: Whist, trick: Trick): State = apply(whist, trick, Tricks.zero.increment(trick))

  /**
    * Method to create an initial state based on a deal.
    *
    * @param whist the Deal of the new State.
    * @return a new State based on the Deal without any tricks having been played.
    */
  def apply(whist: Whist): State = apply(whist, Trick.empty)

  /**
    * CONSIDER moving this into State (or Trick)
    *
    * Method to create a new State based on the outcome of the current trick.
    * If the current trick is complete then we create a new State based on:
    * (1) an update of the current game of whist according to the (quitted) trick, and
    * (2) the new tricks value.
    * If the current trick is not complete, we simply return a new State based on:
    * (1) an update of the current game of whist according to the trick, and
    * (2) the same tricks value.
    *
    * @param whist  the current state of the game we are playing.
    * @param trick  the current trick.
    * @param tricks the current state of the tricks.
    * @return a new State with an updated Whist and, potentially, an updated tricks.
    */
  def create(whist: Whist, trick: Trick, tricks: Tricks): State =
    if (trick.started) {
      if (trick.isComplete) State(whist.play(trick.plays.last).quit, trick, tricks.increment(trick))
      else State(whist.play(trick.plays.last), trick, tricks)
    }
    else throw CardException(s"cannot create a new State based on an empty trick")

  implicit object StateFitness extends Fitness[State] {
    /**
      * Evaluate the heuristic fitness of this state.
      * We add the number of tricks owned by NS to the potential of NS to take more tricks.
      * NOTE: that we ignore the tricks already taken by EW.
      *
      * @param x the value whose fitness is to be evaluated.
      * @return the fitness of x as a Double.
      */
    override def fitness(x: State): Double = x.tricks.ns + x.deal.evaluate
  }

  implicit object StateOrdering extends Ordering[State] {
    /**
      * Compare two States.
      *
      * @param x first State.
      * @param y second State.
      * @return positive number if y occurs after x in the sequence of moves.
      */
    def compare(x: State, y: State): Int = y.cardsPlayed - x.cardsPlayed
  }

  implicit object LoggableState extends Loggable[State] with Loggables {
    def toLog(t: State): String =
      s"${t.trick.history.mkString("", ", ", "")} " +
        //				s"${implicitly[Loggable[Trick]].toLog(t.trick)} " +
        s"${implicitly[Loggable[Tricks]].toLog(t.tricks)} " +
        s"${t.fitness} " +
        s"${implicitly[Loggable[Whist]].toLog(t.whist)}"
  }

  implicit object ShowState extends Show[State] {
    def show(t: State): String = t.neatOutput
  }
}

/**
  * Behavior of something which can be validated.
  */
trait Validatable {
  /**
    * Method to validate this Validatable object.
    *
    * @return true if this object is valid.
    */
  def validate: Boolean
}
