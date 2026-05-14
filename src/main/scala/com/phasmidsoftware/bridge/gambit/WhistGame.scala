package com.phasmidsoftware.bridge.gambit

import com.phasmidsoftware.bridge.cards.*
import com.phasmidsoftware.gambit.game.{Game, State as GState}

/**
  * The Gambit `Game[State, CardPlay, Int]` typeclass instance for Whist card play.
  *
  * Type parameters:
  * S  = State     (bridge game state)
  * M  = CardPlay  (a single card play)
  * Pl = Int       (hand index: 0=N, 1=E, 2=S, 3=W)
  *
  * @param whist the Whist game providing the starting position and opening leader.
  */
class WhistGame(whist: Whist) extends Game[State, CardPlay, Int]:

  /** The initial state of the game. */
  def start: State = State(whist)

  /** North (hand 0) leads first by convention; overridden by `whist.openingLeader`. */
  def startingPlayer: Int = whist.openingLeader

  /** All four hand indices in clockwise order. */
  def players: Seq[Int] = Seq(0, 1, 2, 3)

  /**
    * Legal moves from state `s`: the `CardPlay` that leads to each successor state.
    * Recovered from the last play of each successor's trick.
    */
  def moves(s: State): Seq[CardPlay] =
    s.enumeratePlays.flatMap(_.trick.plays.lastOption)

  /**
    * Apply a `CardPlay` to state `s`.
    * We find the successor state whose last trick play matches `cardPlay`.
    * Falls back to the first successor if no exact match is found (should not happen).
    */
  def applyMove(s: State, cardPlay: CardPlay, pl: Int): State =
    s.enumeratePlays
      .find(_.trick.plays.lastOption.contains(cardPlay))
      .getOrElse(throw new IllegalArgumentException(s"applyMove: CardPlay $cardPlay not found in successors of $s"))

  /**
    * The next player after `current` has moved — simply the next hand clockwise.
    */
  def nextPlayer(s: State, current: Int): Int = Hand.next(current)

  /**
    * The winner determination: the player who just moved (prev) wins (+1),
    * the current player (about to move) loses (-1).
    * For two-partnership scoring, partners share the same result.
    */
  override def winner(s: State, current: Int): Map[Int, Int] =
    val prev = Hand.next(current, 3) // one step back = previous player
    players.map { pl =>
      pl -> (if Hand.sameSide(pl, prev) then 1 else -1)
    }.toMap

object WhistGame:
  /**
    * Convenience factory.
    *
    * @param whist the Whist game.
    * @return a `Game[State, CardPlay, Int]` instance.
    */
  def apply(whist: Whist): Game[State, CardPlay, Int] = new WhistGame(whist)