package com.phasmidsoftware.bridge.gambit

import com.phasmidsoftware.bridge.cards.State
import com.phasmidsoftware.bridge.gambit.WhistState.logger
import com.phasmidsoftware.gambit.game.{Move, Transition, State as GState}
import com.phasmidsoftware.gambit.util.LazyLogger

/**
  * Factory for the Gambit `State[State, State]` typeclass instance for Whist card play.
  *
  * Because `isGoal` depends on the target trick count and direction (which vary per
  * `analyzeDoubleDummy` call), we provide a factory method rather than a single `given`.
  *
  * Usage:
  * {{{
  *   given GState[State, State] = WhistState(neededTricks = 9, directionNS = true)
  * }}}
  *
  * @param neededTricks the number of tricks the protagonists need to achieve their goal.
  * @param directionNS  true if NS are the protagonists, false if EW.
  */
class WhistState(neededTricks: Int, directionNS: Boolean)(using game: WhistGame) extends GState[State, State]:
  val me: Int = if directionNS then 0 else 1

  override def isMaximizing(s: State, currentMaximizing: Boolean): Boolean =
    val result = game.currentPlayer(s)(using this) == me
    logger.debug(s"WhistState.isMaximizing: currentPlayer=${game.currentPlayer(s)(using this)}, me=$me, result=$result")
    result

  override def leafValue(s: State, maximizing: Boolean): Double =
    logger.debug(s"leafValue: maximizing=$maximizing, isGoal=${isGoal(s)}, tricks=${s.tricks}, complete=${s.trick.isComplete}, size=${s.trick.size}, winner=${s.trick.winner.map(_.play.hand)}")
    heuristic(s)

  /**
    * The sequence number of a State — total cards played so far.
    * Used by `isFirstPlayerToMove` to determine whose turn it is.
    */
  def sequence(s: State): Int = s.cardsPlayed

  /**
    * Identity construction: the proto-state and state are the same type.
    */
  def construct(proto: (State, State)): State = proto._1

  /**
    * All bridge States are structurally valid.
    */
  def isValid(s: State): Boolean = true

  /**
    * A win means the goal has been achieved (protagonist side reached neededTricks).
    */
  def isWin(s: State): Boolean = isGoal(s).contains(true)

  // in WhistState
  var maxNSTricks: Int = 0

  /**
    * Goal detection, incorporating early termination:
    *
    * - `Some(true)`  — protagonists have reached `neededTricks`
    * - `Some(false)` — opponents have made it impossible (counter-goal), OR
    *   there are insufficient moves remaining to reach the goal
    *   (port of `WhistGoalDriven.goalImpossible`)
    * - `None`        — game still in progress
    *
    * The `goalImpossible` check is critical for performance: without it the
    * search explores branches that can never achieve the goal, making the
    * search intractable on a full 52-card deal.
    */
  def isGoal(s: State): Option[Boolean] =
    if s.tricks.ns > maxNSTricks then maxNSTricks = s.tricks.ns
    if !s.trick.isComplete && s.trick.size != 0 then return None
    val movesRemaining = s.whist.deal.nCards
    val history = s.trick.history
    //    logger.debug(s"isGoal: deal.nCards=${s.whist.deal.nCards}, trick.cardsPlayed=${s.trick.cardsPlayed}, tricks=${s.tricks}")
    val decided = s.tricks.decide(neededTricks, directionNS)
    val sufficient = s.trick.sufficientMovesRemaining(movesRemaining, directionNS, neededTricks, s.tricks)
    //    if (movesRemaining % 4 == 0)
    //      logger.debug(s"isGoal: (4 | movesRemaining) tricks=${s.tricks}, cardsPlayed=${s.cardsPlayed}, movesRemaining=$movesRemaining, decided=$decided, sufficient=$sufficient")
    val result = decided match
      case Some(x) if x =>
//        val history = s.trick.history
//        logger.debug(s"SUCCESS: $history")
        Some(x)
      case Some(x) =>
        Some(x)
      case None =>
        if !sufficient then Some(false)
        else
          //          logger.debug(s"isGoal=None at tricks=${s.tricks}, cardsPlayed=${s.cardsPlayed}, movesRemaining=$movesRemaining, decided=$decided, sufficient=$sufficient")
          None
    result.foreach {
      r => logger.debug(s"isGoal=$r at tricks=${s.tricks}, cardsPlayed=${s.cardsPlayed}, movesRemaining=$movesRemaining, decided=$decided, sufficient=$sufficient")
    }
    logger.debug(s"isGoal: result=$result: $history")
    result

  /**
    * Heuristic from the perspective of the player who just moved.
    *
    * For terminal positions returns ±`Double.MaxValue`.
    * For non-terminal positions returns the NS fitness (tricks taken + potential),
    * negated when it is EW's perspective.
    *
    * The sign convention follows Gambit: positive = good for the player who just moved.
    * `isFirstPlayerToMove` returns true when `cardsPlayed % 2 == 0`, meaning the
    * opening leader (hand 0) moves on even plies. For a fuller multi-player game
    * this would need to track the current hand index; here we use NS/EW parity.
    */
  def heuristic(s: State): Double =
    isGoal(s) match
      case Some(true) =>
        logger.debug(s"heuristic: isGoal=true, tricks=${s.tricks}, winner=${s.trick.winner.map(_.play.hand)}, isNS=${s.trick.winner.map(_.play.hand % 2 == 0)}")
        Double.MaxValue
      case Some(false) => -Double.MaxValue
      case None => State.heuristicFitness(s)

  //    val lastHand = s.trick.plays.lastOption
  //      .orElse(s.trick.maybePrior.flatMap(_.plays.lastOption))
  //      .map(_.hand)
  //    val nsJustMoved = lastHand.map(_ % 2 == 0).getOrElse(true) // true = NS, only at game start where heuristic is approximate anyway
  //    val nsScore = isGoal(s) match
  //      case Some(true) =>
  //        logger.debug(s"heuristic: isGoal=true, tricks=${s.tricks}, winner=${s.trick.winner.map(_.play.hand)}, isNS=${s.trick.winner.map(_.play.hand % 2 == 0)}")
  //        Double.MaxValue
  //      case Some(false) =>
  //        -Double.MaxValue
  //      case None => State.heuristicFitness(s)
  //    if nsJustMoved then nsScore else -nsScore

  //    isGoal(s) match
  //      case Some(true) =>
  //        logger.debug(s"heuristic: isGoal=true, tricks=${s.tricks}, winner=${s.trick.winner.map(_.play.hand)}, isNS=${s.trick.winner.map(_.play.hand % 2 == 0)}")
  //        Double.MaxValue // NS wins — always good for NS
  //      case Some(false) =>
  //        -Double.MaxValue // NS loses — always bad for NS        if isNSToMove(s) then Double.MaxValue else -Double.MaxValue
  //      case None =>
  //          val raw = State.heuristicFitness(s)
  //          if isNSToMove(s) then raw else -raw

  /**
    * Legal moves: one Transition per successor State reachable from `s`.
    * Each successor's last card play is recovered as the move description.
    */
  def moves(s: State): Seq[Transition[State, State]] =
    s.enumeratePlays.map { successor =>
      Move[State, State](
        f = _ => successor,
        //        desc = successor.trick.plays.lastOption.map(_.toString).getOrElse("?")
        desc = successor.lastPlay.map(_.toString).getOrElse("?"))
    }

  def render(s: State): String = s.neatOutput

object WhistState:
  private val logger = LazyLogger(getClass)