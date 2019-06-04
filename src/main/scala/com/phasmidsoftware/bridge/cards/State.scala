/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.Fitness
import com.phasmidsoftware.output.{Output, Outputable}

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

	// TODO remove this.
	State.count += 1

	//	println(s"State: ${State.count} ${whist.deal.title} ${trick.index} ${trick.plays.lastOption} ${tricks.ns} ${tricks.ew}")

	/**
		* The goal method for this State.
		*
		* @param directionNS  true if the direction of the declarer is NS.
		* @param declarerGoal the declarer's goal in terms of tricks--once declarer reaches this goal OR...
		*                     the opponents make this goal impossible, then the goal is achieved.
		* @param tricksToPlay the total number of tricks to play, usually the value of Deal.TricksPerDeal, i.e. 13.
		* @return true if the goal has been reached.
		*/
	def goal(directionNS: Boolean, declarerGoal: Int, tricksToPlay: Int): Boolean = {
		val tuple = declarerGoal -> (tricksToPlay + 1 - declarerGoal)
		val f = (tricks.goal _).tupled
		f(if (directionNS) tuple else tuple.swap)
	}

	/**
		* Method to enumerate all of the possible states that could be children of the Node enclosing this State.
		*
		* @return a sequence of States.
		*/
	def enumeratePlays: Seq[State] = _enumeratePlays

	//	TODO make private
	lazy val enumerateFollows: Seq[State] = trick.next match {
		case Some(t) => whist.makeStates(tricks, for (p <- deal.hands(t).choosePlays(trick)) yield trick :+ p)
		case None => throw CardException(s"State: $this cannot be followed")
	}

	//	TODO make private
	def enumerateLeads(leader: Int, index: Int): Seq[State] =
		whist.makeStates(tricks, enumerateLeadsAsTricks(leader, index))

	//	TODO make private
	// CONSIDER desugaring the body
	def enumerateLeadsAsTricks(leader: Int, index: Int): Seq[Trick] =
		for (p <- chooseLeads(leader)) yield Trick(index, Seq(p))

	// TODO make private
	def chooseLeads(leader: Int): Seq[CardPlay] =
		deal.hands(leader).longestSuit.choosePlays(deal, leader, FourthBest, None)

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
		*
		* Method to validate this State.
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
	def neatOutput: String = s"State: $trick $tricks $fitness ${deal.neatOutput}"

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

	private lazy val _isConsistent = trick.cardsPlayed + deal.cards == 52 // && validate
}

object State {
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
		* Method to generate a goal function of type State => Boolean.
		* The function required depends on the three parameters passed in, i.e. it closes on these three parameters.
		*
		* @param directionNS  true if the direction of the declarer is NS.
		* @param declarerGoal the declarer's goal in terms of tricks--once declarer reaches this goal OR...
		*                     the opponents make this goal impossible, then the goal is achieved.
		* @param tricksToPlay the total number of tricks to play, usually the value of Deal.TricksPerDeal, i.e. 13.
		* @return true if the goal has been reached.
		*/
	def goalFunction(directionNS: Boolean, declarerGoal: Int, tricksToPlay: Int = Deal.TricksPerDeal): State => Boolean = { s =>
		(s.goal _).tupled((directionNS, declarerGoal, tricksToPlay))
	}

	/**
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

	// TODO remove this.
	var count = 0
}

/**
	* This class represents the current trick totals for the two partnerships.
	*
	* @param ns the number of tricks NS has taken.
	* @param ew the number of tricks EW has taken.
	*/
case class Tricks(ns: Int, ew: Int) extends Evaluatable {
	/**
		* Increment the total according to the value of winner.
		* CONSIDER using Hand.sameSide
		*
		* @param winner the index of the winner of a trick.
		* @return a new version of Tricks.
		*/
	def increment(winner: Int): Tricks = if (winner % 2 == 0) incNS else incEW

	/**
		* Increment the total according to the play of the trick given.
		* Note that, increment(Int) is called only if the trick is complete.
		* Otherwise, this is returned.
		*
		* @param trick the trick.
		* @return a new version of Tricks.
		*/
	def increment(trick: Trick): Tricks = trick.winner match {
		case Some(Winner(p, true)) => increment(p.hand)
		case _ => this
	}

	/**
		* Method to determine if this instance of Tricks satisfies the goal.
		* Once the goal is reached, we stop expanding the state tree.
		*
		* @param nsTricks the number of NS tricks which will trigger the goal when reached.
		* @param ewTricks the number of EW tricks which will trigger the goal when reached.
		* @return true if the goal has been reached, otherwise false.
		*/
	def goal(nsTricks: Int, ewTricks: Int): Boolean = ns >= nsTricks || ew >= ewTricks

	/**
		* @return a Double representing the value of this Tricks.
		*/
	def evaluate: Double = _evaluate

	lazy val incNS: Tricks = Tricks(ns + 1, ew)

	lazy val incEW: Tricks = Tricks(ns, ew + 1)

	override def toString: String = s"$ns:$ew"

	private lazy val _evaluate = ns - ew
}

object Tricks {
	val zero = Tricks(0, 0)
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
