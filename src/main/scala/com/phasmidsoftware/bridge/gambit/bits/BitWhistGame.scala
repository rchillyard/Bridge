package com.phasmidsoftware.bridge.gambit.bits

import com.phasmidsoftware.bridge.cards.bits.{BitState, TrickPlay}
import com.phasmidsoftware.gambit.game.{Game, State as GState}

/**
  * The Gambit `Game[BitState, TrickPlay, Int]` typeclass instance for the bitboard Whist
  * engine, mirroring `com.phasmidsoftware.bridge.gambit.WhistGame`'s shape but delegating
  * straight to `BitState.legalPlays`/`play` -- an O(1) bit-clear per move, not a
  * build-then-discard-every-child-state pass over the object graph.
  *
  * Type parameters: S = M = BitState's own type (self-referential, as in `WhistGame`);
  * M = TrickPlay; Pl = Int (hand index: 0=N, 1=E, 2=S, 3=W).
  */
class BitWhistGame extends Game[BitState, TrickPlay, Int]:

  def start: BitState = throw new UnsupportedOperationException(
    "BitWhistGame.start is not used by BitAnalysis.analyzeDoubleDummy, which constructs its own initial BitState")

  def startingPlayer: Int = 0

  def players: Seq[Int] = Seq(0, 1, 2, 3)

  def moves(s: BitState): Seq[TrickPlay] = s.legalPlays

  def applyMove(s: BitState, m: TrickPlay, pl: Int): BitState = s.play(m)

  def nextPlayer(s: BitState, current: Int): Int = (current + 1) % 4

  /**
    * Normalized to partnership (0 = NS, 1 = EW), matching `me`'s scale in `BitWhistState`/
    * `AlphaBetaPlayer` -- NOT the raw hand index. `WhistGame.currentPlayer` does the same
    * `seat % 2` normalization; returning the raw hand index here (as an earlier version of
    * this method did) makes `isMaximizing` compare a partnership value against a seat value,
    * so it only ever matches by coincidence -- e.g. correct for North (seat 0) but inverted
    * for South (seat 2), silently treating the declaring side's own South-hand decisions as
    * working against it.
    */
  override def currentPlayer[P](s: BitState)(using state: GState[P, BitState]): Int = s.currentPlayer % 2

  override def winner(s: BitState, current: Int): Map[Int, Int] =
    val prev = (current + 3) % 4
    players.map(pl => pl -> (if pl % 2 == prev % 2 then 1 else -1)).toMap
