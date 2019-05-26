package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.Fitness
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

/**
	* Case class State to represent a possible state of play in analysis of a Deal.
	* The state must be consistent, which is to say that the deal is the result of playing all of the previous tricks AND
	* the current (possibly partial) trick.
	*
	* @param deal  the Deal.
	* @param trick the Trick.
	*/
case class State(deal: Deal, trick: Trick, tricks: Tricks) extends Outputable[Unit] with Validatable {

	/**
		* Initialization: just checking that this State is consistent.
		*
		* TODO remove this check.
		*/
	if (!isConsistent) System.err.println(s"state not consistent ${deal.cards}, $deal: $trick")

	/**
		* Method to enumerate all of the possible states that could be children of the Node enclosing this State.
		*
		* @return a sequence of States.
		*/
	def enumeratePlays: Seq[State] = _enumeratePlays

	//	TODO make private
	lazy val enumerateFollows: Seq[State] =
		Tree.makeStates(deal, tricks, for (p <- deal.hands(trick.next).choosePlays(trick)) yield trick :+ p)

	//	TODO make private
	def enumerateLeads(leader: Int, index: Int): Seq[State] =
		Tree.makeStates(deal, tricks, for (p <- chooseLead(leader)) yield Trick(index, Seq(p), leader, p.suit))

	// TODO make private
	def chooseLead(leader: Int): Seq[CardPlay] = deal.hands(leader).longestSuit.choosePlays(deal, leader, FourthBest)

	/**
		* Method to get the next State in sequence.
		*
		* @param t the next Trick.
		* @return a new consistent State based on deal and t.
		*/
	def next(t: Trick): State = State.create(deal, t, tricks)

	/**
		* @return true if the number of cards played according to the trick plus the number of cares remaining in the deal equals 52
		*/
	def isConsistent: Boolean = _isConsistent

	/**
		* Method to validate this State.
		*
		* @return true if all the plays of the trick are validated
		*/
	def validate: Boolean = _validate

	/**
		* The total number of cards played according to
		*
		* @return
		*/
	lazy val cardsPlayed: Int = trick.index * 4 + trick.size

	/**
		* @return the fitness of this State rounded to the nearest 0.1
		*/
	lazy val fitness: Double = math.rint(State.StateFitness.fitness(this) * 10) / 10

	override def toString: String = s"State: $trick $fitness ${deal.neatOutput}"

	/**
		* Invokes output on the trick, passing it Some(deal) and appending the fitness in parentheses.
		* @param output the output to append to.
		* @param xo     an optional value of X, defaulting to None.
		* @return a new instance of Output.
		*/
	def output(output: Output, xo: Option[Unit] = None): Output = trick.output(output, Some(deal)) :+ s" ($fitness)"

	private lazy val _enumeratePlays = trick.winner match {
		case Some(winner) => enumerateLeads(winner, trick.index + 1)
		case None => enumerateFollows
	}

	private lazy val _validate = trick.plays.map(_.validate).forall(_ == true)

	private lazy val _isConsistent = cardsPlayed + deal.cards == 52 // && validate
}

object State {
	/**
		* Method to create an initial state based on a deal.
		*
		* TODO we need a better representation of a non-trick, for example with a non-suit defined.
		*
		* @param deal the Deal of the new State.
		* @return a new State based on the Deal without any tricks having been played.
		*/
	def apply(deal: Deal, trick: Trick): State = apply(deal, trick, Tricks.zero.increment(trick))

	/**
		* Method to create an initial state based on a deal.
		*
		* TODO we need a better representation of a non-trick, for example with a non-suit defined.
		*
		* @param deal the Deal of the new State.
		* @return a new State based on the Deal without any tricks having been played.
		*/
	def apply(deal: Deal): State = apply(deal, Trick(0, Nil, 0, Spades))

	def create(deal: Deal, trick: Trick, tricks: Tricks): State =
		if (trick.isComplete) State(deal.play(trick.plays.last).quit, trick, tricks.increment(trick))
		else State(deal.play(trick.plays.last), trick, tricks)

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
}

case class Tricks(ns: Int, ew: Int) extends Evaluatable {
	/**
		* @return a Double representing the value of this Tricks.
		*/
	def evaluate: Double = _evaluate

	lazy val incNS: Tricks = Tricks(ns + 1, ew)

	lazy val incEW: Tricks = Tricks(ns, ew + 1)

	def increment(winner: Int): Tricks = if (winner % 2 == 0) incNS else incEW

	def increment(trick: Trick): Tricks = trick.winner match {
		case Some(w) => increment(w)
		case None => this
	}

	override def toString: String = s"$ns:$ew"

	private lazy val _evaluate = ns - ew
}

object Tricks {
	val zero = Tricks(0, 0)
}

trait Validatable {
	/**
		* Method to validate this Validatable object.
		*
		* @return true if this object is valid.
		*/
	def validate: Boolean
}