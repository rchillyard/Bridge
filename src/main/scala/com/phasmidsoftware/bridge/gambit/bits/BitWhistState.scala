package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.bits.BitState
import com.phasmidsoftware.gambit.game.{Move, State as GState, Transition}

/**
  * The Gambit `State[BitState, BitState]` typeclass instance for the bitboard Whist engine,
  * mirroring `com.phasmidsoftware.bridge.gambit.WhistState`'s shape (a factory, since
  * `isGoal` depends on the per-call target trick count and direction).
  *
  * @param neededTricks the number of tricks the protagonists need to achieve their goal.
  * @param directionNS  true if NS are the protagonists, false if EW.
  */
class BitWhistState(neededTricks: Int, directionNS: Boolean)(using game: BitWhistGame) extends GState[BitState, BitState]:
  private val me: Int = if directionNS then 0 else 1

  override def isMaximizing(s: BitState, currentMaximizing: Boolean): Boolean =
    game.currentPlayer(s)(using this) == me

  override def leafValue(s: BitState, maximizing: Boolean): Double = heuristic(s)

  def sequence(s: BitState): Int = s.cardsPlayed

  def construct(proto: (BitState, BitState)): BitState = proto._1

  def isValid(s: BitState): Boolean = true

  def isWin(s: BitState): Boolean = isGoal(s).contains(true)

  def isGoal(s: BitState): Option[Boolean] = s.isGoal(neededTricks, directionNS)

  def heuristic(s: BitState): Double = isGoal(s) match
    case Some(true) => Double.MaxValue
    case Some(false) => -Double.MaxValue
    case None => s.heuristic

  def moves(s: BitState): Seq[Transition[BitState, BitState]] =
    s.legalPlays.map(p => Move[BitState, BitState](f = _ => s.play(p), desc = p.toString))

  def render(s: BitState): String = s.toString
