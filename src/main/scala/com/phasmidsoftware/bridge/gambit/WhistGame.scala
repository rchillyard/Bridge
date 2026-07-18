package com.phasmidsoftware.bridge.gambit

import com.phasmidsoftware.bridge.cards.*
import com.phasmidsoftware.gambit.game.{Game, State as GState}
import com.phasmidsoftware.gambit.util.LazyLogger

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
    s.enumeratePlays.flatMap(_.lastPlay)

  /**
    * Apply a `CardPlay` to state `s`.
    * We find the successor state whose last trick play matches `cardPlay`.
    * Falls back to the first successor if no exact match is found (should not happen).
    */
  def applyMove(s: State, cardPlay: CardPlay, pl: Int): State =
    val candidates = s.enumeratePlays
    val found = candidates.find(_.lastPlay.contains(cardPlay))
    //    WhistGame.logger.debug(s"applyMove: cardPlay=$cardPlay, candidates=${candidates.size}, found=${found.isDefined}, lastPlays=${candidates.map(_.lastPlay)}")
    found.getOrElse(throw new IllegalArgumentException(s"applyMove: CardPlay $cardPlay not found in successors of $s"))

  /**
    * The next player after `current` has moved — simply the next hand clockwise.
    */
  def nextPlayer(s: State, current: Int): Int = Hand.next(current)

  /**
    * Determines the current player based on the current state of the game.
    * The method resolves the seat index of the player, normalizes it to a partnership value,
    * and logs the computation details for debugging purposes.
    *
    * @param s     the current state of the game, containing the trick details and other information about the game status.
    * @param state an implicit game state context required for evaluation.
    * @return an integer representing the normalized partnership: 0 for North-South (NS) or 1 for East-West (EW).
    */
  override def currentPlayer[P](s: State)(using state: GState[P, State]): Int =
    val seat = if s.trick.isComplete then
      s.trick.winner.map(_.play.hand).getOrElse(startingPlayer)
    else
      s.trick.leader.map(l => (l + s.trick.size) % 4).getOrElse(startingPlayer)
    val result = if seat % 2 == 0 then 0 else 1 // normalize to partnership: 0=NS, 1=EW
    WhistGame.logger.debug(s"WhistGame.currentPlayer: seat=$seat, complete=${s.trick.isComplete}, size=${s.trick.size}, winner=${s.trick.winner}, leader=${s.trick.leader}, result=$result")
    result

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
  //  def apply(whist: Whist): WhistGame = new WhistGame(whist)

  private val logger = LazyLogger(getClass)
