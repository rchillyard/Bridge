/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.decisiontree.{Expandable, GoalDriven}
import com.phasmidsoftware.util._

import scala.language.implicitConversions

/**
  * This class represents a game of Whist.
  * In Whist, there are four players around a table.
  * Players sitting opposite each other are part of the same "team." They are said to be partners.
  * Play goes clockwise around the table, each player contributing one card, and the team which contributed the highest card (or possibly the highest trump)
  * is credited with that "trick".
  * There being 52 cards in a deck (pack), there will be 13 tricks.
  * The player (not the team) who wins one trick must lead to the following trick.
  * At the start of the game, it is arbitrary which player is the opening leader--there are various schemes to determine
  * who leads, most notably Auction and Contract Bridge designate the player sitting on "declarer's" left.
  * Here, however, the opening leader is simply determined by parameter.
  *
  * The particular arrangement (shuffle) of the cards is determined by the deal parameter.
  *
  * @param deal          the arrangement of cards.
  * @param openingLeader the player on opening lead (0 thru 3 for "North" thru "West").
  */
case class Whist(deal: Deal, openingLeader: Int) extends Playable[Whist] with Quittable[Whist] {

  /**
    * Method to make a sequence of States from the given sequence of Trick instances.
    *
    * NOTE: this originally had a filter that removed States that did not have a high fitness.
    *
    * @param tricks the current value of Tricks (i.e. current score NS vs. EW).
    * @param ts     a sequence of Trick instances.
    * @return a sequence of State objects corresponding to the values of ts.
    */
  def makeStates(tricks: Tricks, ts: List[Trick]): List[State] = ts.map(t => State.create(this, t, tricks))

  /**
    * Play a card from this Playable object.
    *
    * @param cardPlay the card play.
    * @return a new Playable.
    */
  def play(cardPlay: CardPlay): Whist = Whist(deal.play(cardPlay), openingLeader)

  /**
    * Solve this Whist game as a double-dummy problem where one side or the other (depending on directionNS)
    * attempts to reach a total of tricks. As soon as our protagonists have reached the trick total, all expansion will cease.
    * When the opponents have made it impossible for the protagonists to reach said trick total, all expansion will cease.
    *
    * @param tricks      the number of tricks required.
    * @param directionNS if true then the direction we care about is NS else EW.
    * @return an optional State which indicates the first "solution" found.
    *         It may represent success or failure on the part of the protagonists.
    *         If the result is None, it means that no solution of any sort was found.
    */
  def analyzeDoubleDummy(tricks: Int, directionNS: Boolean): Option[Boolean] = {
    implicit val sg: GoalDriven[State] = Whist.goal(tricks, directionNS)
    implicit val se: Expandable[State] = (t: State) => t.enumeratePlays
    val tree = StateTree(this)
    val node = tree.expand()
//    node.output(Output(System.out)).insertBreak().close()
    node.so flatMap (sn => sn.tricks.decide(tricks, directionNS))
  }

  /**
    * Method to enact the pending promotions on this Quittable.
    *
    * CONSIDER making this a lazy val.
    *
    * @return an eagerly promoted Whist game.
    */
  def quit: Whist = Whist(deal.quit, openingLeader)

  /**
    * Create an initial state for this Whist game.
    *
    * NOTE: only used in unit testing.
    *
    * @return a State using deal and openingLeader
    */
  lazy val createState: State = State(this)
}

/**
  * Trait to customize the behavior of GoalDriven for a whist/bridge game.
  */
trait WhistGoalDriven extends GoalDriven[State] {
  val neededTricks: Int
  val directionNS: Boolean
  val totalTricks: Int

  def goalAchieved(t: State): Boolean = t.tricks.decide(neededTricks, directionNS) match {
    case Some(_) => true // We ignore the Boolean value for now.
    case None => false
  }

  def goalImpossible(t: State, moves: Int): Boolean = moves < t.trick.movesRequired(directionNS, neededTricks, t.tricks)
}

object Whist {

  implicit object LoggableWhist extends Loggable[Whist] with Loggables {
    def toLog(t: Whist): String = s"${implicitly[Loggable[Deal]].toLog(t.deal)}@${Deal.name(t.openingLeader)}"
  }

  def goal(_neededTricks: Int, _directionNS: Boolean, _totalTricks: Int = Deal.TricksPerDeal): WhistGoalDriven = new WhistGoalDriven {
    val neededTricks: Int = _neededTricks
    val directionNS: Boolean = _directionNS
    val totalTricks: Int = _totalTricks
  }

}

/**
  * The behavior of this trait is to (eagerly) quit a trick (holding, sequence),
  * which is to say take the (lazy) promotions of a sequence and to promote them eagerly according to the
  * quitting of the current trick.
  *
  * @tparam X the underlying type.
  */
trait Quittable[X] {
  /**
    * Method to enact the pending promotions on this Quittable.
    *
    * @return an eagerly promoted X.
    */
  def quit: X
}
/**
  * Trait to model the behavior of play-choosing strategy.
  * We aim to choose the most favorable play each time so that we can achieve our goal quicker.
  *
  * In general, we check these values in the same sequence as they are defined below.
  */
trait Strategy {
  /**
    * @return true if the card played depends on its whether we can beat the current winner;
    *         false if we always play the same card.
    */
  val conditional: Boolean

  /**
    * @return true if we want to try to win the trick if possible.
    *         false if we are OK with not winning the trick.
    */
  val win: Boolean

}

abstract class BaseStrategy(val win: Boolean, val conditional: Boolean) extends Strategy

case object WinIt extends BaseStrategy(true, false)

case object LeadHigh extends BaseStrategy(true, false)

case object Cover extends BaseStrategy(false, true)

case object Finesse extends BaseStrategy(true, true)

case object FourthBest extends BaseStrategy(false, false)

case object Duck extends BaseStrategy(false, false)

case object Discard extends BaseStrategy(false, false)

/**
  * Trait to describe behavior of a type which can experience the play of a card.
  *
  * For example, Holding, Sequence, etc. can have cards played.
  *
  * NOTE: in practice, this trait is implemented via hierarchy, not type-class.
  *
  * @tparam X the underlying type.
  */
trait Playable[X] {
  /**
    * Play a card from this Playable object.
    *
    * @param cardPlay the card play.
    * @return a new Playable.
    */
  def play(cardPlay: CardPlay): X
}

/**
  * Trait to model the property of being (heuristically) evaluated.
  */
trait Evaluatable {

  /**
    * Evaluate this Evaluatable object for its (heuristic) trick-taking capability.
    *
    * @return a Double
    */
  def evaluate: Double
}

trait Removable {
  /**
    * Method to remove an element of the appropriate priority from a Removable.
    *
    * CONSIDER renaming this and also adding a suit parameter so that Hand can define it.
    *
    * @param priority the priority.
    * @return a new Removable without an element of the given priority.
    */
  //noinspection ScalaStyle
  def -(priority: Int): Removable
}
