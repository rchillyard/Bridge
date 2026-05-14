package com.phasmidsoftware.bridge.gambit

import com.phasmidsoftware.bridge.cards.*
import com.phasmidsoftware.gambit.game.{Move, Transition, State as GState}

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
class WhistState(neededTricks: Int, directionNS: Boolean) extends GState[State, State]:

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

  /**
    * Goal detection:
    * - `Some(true)`  — protagonists have reached `neededTricks`
    * - `Some(false)` — opponents have made it impossible (counter-goal reached), or all 13 tricks played
    * - `None`        — game still in progress
    */
  def isGoal(s: State): Option[Boolean] =
    s.tricks.decide(neededTricks, directionNS) match
      case Some(x) => Some(x)
      case None => if s.tricks.ns + s.tricks.ew == 13 then Some(false) else None

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
      case Some(true) => if isNSToMove(s) then Double.MaxValue else -Double.MaxValue
      case Some(false) => if isNSToMove(s) then -Double.MaxValue else Double.MaxValue
      case None =>
        val raw = State.heuristicFitness(s)
        if isNSToMove(s) then raw else -raw

  /**
    * Legal moves: one Transition per successor State reachable from `s`.
    * Each successor's last card play is recovered as the move description.
    */
  def moves(s: State): Seq[Transition[State, State]] =
    s.enumeratePlays.map { successor =>
      Move[State, State](
        f = _ => successor,
        desc = successor.trick.plays.lastOption.map(_.toString).getOrElse("?")
      )
    }

  def render(s: State): String = s.neatOutput

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  /**
    * True when NS is the side that just moved into `s`.
    * Uses cardsPlayed parity: NS leads on ply 0 (and every 4th ply), but for
    * heuristic purposes we treat NS as having just moved on odd cardsPlayed
    * (i.e. a card was just played by an NS hand).
    *
    * NOTE: This is an approximation for the two-partnership case.
    * A fully correct implementation would track the actual hand index.
    */
  private def isNSToMove(s: State): Boolean = s.cardsPlayed % 2 == 1

object WhistState:
  /**
    * Convenience factory.
    *
    * @param neededTricks tricks required by the protagonists.
    * @param directionNS  true if NS are the protagonists.
    * @return a `GState[State, State]` instance for use as a `given`.
    */
  def apply(neededTricks: Int, directionNS: Boolean): GState[State, State] =
    new WhistState(neededTricks, directionNS)