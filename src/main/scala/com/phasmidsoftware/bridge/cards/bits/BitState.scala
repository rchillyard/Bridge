/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import com.phasmidsoftware.bridge.cards.{CacheKey, Tricks}

/**
  * A double-dummy search state expressed entirely in bitboard terms -- no `Deal`/`Hand`/
  * `Holding`/`Trick` object graph anywhere on the hot path. This is a new, self-contained
  * engine that runs alongside the existing object-graph engine (`State`/`Whist`/`WhistGame`),
  * not a replacement for it: it is validated against the existing engine's known-correct
  * results (see `BitWhistValidationSpec`) before any decision to cut over.
  *
  * Deliberate simplifications versus the object-graph engine (all favor correctness/safety
  * over search efficiency, since this is a first validation pass):
  *
  *   - Move ordering is left to Gambit's own top-level heuristic-based reordering
  *     (`AlphaBetaPlayer.orderedMoves`) rather than porting the `Strategy` system
  *     (`Cover`/`Duck`/`Finesse`/`Ruff`/`Discard`/etc.) that the object-graph engine uses to
  *     order and, for discards, to prune to a single "always discard the worst card" candidate.
  *     This engine instead offers EVERY equivalence-class representative in every legal
  *     suit for a discard/ruff, which is strictly more (never fewer) legal candidates than
  *     the old engine tries -- correct by construction, just not as tightly pruned.
  *   - `isGoal`'s "declaring side cannot possibly reach the target" pruning uses only the
  *     unconditionally-safe arithmetic check (`cards remaining >= cards mathematically
  *     required`), omitting the object-graph engine's additional `declaringSideCanWin`
  *     tightening. That extra tightening prunes more aggressively but is harder to verify
  *     bit-for-bit; the weaker check here can only be slower to prove "false", never wrong.
  *   - The non-terminal heuristic is simply tricks-banked-so-far (NS minus EW), not the
  *     object-graph engine's card-potential evaluation (`Deal.evaluate`). This only affects
  *     move-ordering quality and unproven ("Partial") fallback estimates, never a proven
  *     ("Exact") result, since `isGoal` overrides the heuristic wherever it fires.
  *
  * @param deal       the live cards remaining in each of the four hands.
  * @param strain     the trump suit index (0..3), or `None` for notrump.
  * @param leader     the hand on lead for the CURRENT trick.
  * @param trickPlays the plays made so far in the current (possibly empty, never complete --
  *                   see `play`) trick.
  * @param tricks     tricks won by NS/EW in tricks completed so far.
  */
case class BitState(deal: DealBits, strain: Option[Int], leader: Int, trickPlays: Seq[TrickPlay], tricks: Tricks):

  /** The hand whose turn it is to play next. */
  def currentPlayer: Int = (leader + trickPlays.size) % 4

  /** Total cards played so far in the whole deal (used as Gambit's `State.sequence`). */
  def cardsPlayed: Int = (tricks.ns + tricks.ew) * 4 + trickPlays.size

  private def ledSuit: Int = trickPlays.head.suitIndex

  /**
    * Legal moves from this state: one representative `TrickPlay` per equivalence class,
    * per [[SuitMask.equivalenceClasses]]. Must-follow-suit if not void in the led suit;
    * otherwise any card from any other suit (discard or ruff) is legal.
    */
  def legalPlays: Seq[TrickPlay] =
    val player = currentPlayer

    def playsInSuit(suitIndex: Int): Seq[TrickPlay] =
      deal.equivalenceClasses(player, suitIndex).map(cls => TrickPlay(player, suitIndex, cls.topRank))

    if trickPlays.isEmpty then
      (0 until 4).filter(s => deal.hand(player).suitMask(s).nonEmpty).flatMap(playsInSuit)
    else
      val suit = ledSuit
      if deal.hand(player).suitMask(suit).nonEmpty then playsInSuit(suit)
      else (0 until 4).filterNot(_ == suit).filter(s => deal.hand(player).suitMask(s).nonEmpty).flatMap(playsInSuit)

  /**
    * Apply one legal play, returning the successor state. When this completes the trick,
    * the winner becomes the new leader and `trickPlays` resets to empty in the SAME step
    * (there is no separate "trick complete, next leader not yet chosen" state, unlike the
    * object-graph engine's `Trick` -- see the class doc for why this doesn't change
    * `isGoal` semantics).
    */
  def play(p: TrickPlay): BitState =
    val newDeal = deal.play(p.handIndex, p.suitIndex, p.rank)
    val newPlays = trickPlays :+ p
    if newPlays.size == 4 then
      val winner = TrickBits.winningPlay(newPlays, newPlays.head.suitIndex, strain)
      BitState(newDeal, strain, winner.handIndex, Nil, tricks.increment(winner.handIndex))
    else
      BitState(newDeal, strain, leader, newPlays, tricks)

  /** The provisional winner of the trick in progress, if any card has been played to it. */
  def provisionalWinner: Option[TrickPlay] =
    if trickPlays.isEmpty then None else Some(TrickBits.winningPlay(trickPlays, ledSuit, strain))

  /**
    * True if some hand still to play in the CURRENT trick could beat the provisional
    * winner, either with a higher card in the led suit or by ruffing. Ported from
    * `Trick.canSubsequentPlayWin`; used only as a heuristic input (see class doc).
    */
  def canSubsequentPlayWin: Boolean =
    provisionalWinner exists { w =>
      val suit = ledSuit
      val remaining = (trickPlays.size until 4).map(i => (leader + i) % 4)
      remaining.exists { h =>
        val inSuit = deal.hand(h).suitMask(suit)
        val canBeatInSuit = inSuit.nonEmpty && inSuit.topRank > w.rank
        val canRuff = strain.exists(trump =>
          trump != suit && deal.hand(h).suitMask(suit).isEmpty && deal.hand(h).suitMask(trump).nonEmpty)
        canBeatInSuit || canRuff
      }
    }

  /**
    * Goal detection, checked only between tricks (never mid-trick) -- see class doc for why
    * that is exactly one checkpoint per trick here, matching the object-graph engine's
    * `WhistState.isGoal` despite this engine not keeping a stale "just completed" state around.
    *
    * `Some(true)`/`Some(false)` once the target is reached or ruled out; `None` otherwise.
    */
  def isGoal(neededTricks: Int, directionNS: Boolean): Option[Boolean] =
    if trickPlays.nonEmpty then None
    else
      tricks.decide(neededTricks, directionNS) match
        case Some(x) => Some(x)
        case None =>
          val side = if directionNS then tricks.ns else tricks.ew
          val requiredMoves = (neededTricks - side) * 4
          if deal.nCards >= requiredMoves then None else Some(false)

  /**
    * NS-centric heuristic for non-terminal positions: tricks banked so far, scaled well
    * inside (-0.5, 0.5) -- `Whist`'s aspiration window `AlphaBetaWindow(-0.5, 0.5)` (which
    * `BitAnalysis` reuses) assumes any non-terminal value stays inside that window, with
    * only a PROVEN `isGoal` result (`heuristic` above returns `±Double.MaxValue` for those,
    * overriding this) allowed to fall outside it. An unscaled integer trick-count would
    * blow past +-0.5 as soon as either side banks even one more trick than the other,
    * causing the narrow-window search to clamp/cut off before reaching a real conclusion.
    * The max possible trick difference is `Deal.TricksPerDeal` (13); 0.03/trick keeps the
    * worst case (0.39) safely inside the window.
    */
  def heuristic: Double = (tricks.ns - tricks.ew).toDouble * 0.03

  /**
    * A transposition-table key. `deal.hands` (each a `HandBits`) already uses only bits
    * 0-51 of its `Long` (`suitIndex*13 + rank` maxes out at 3*13+12 = 51), leaving bits
    * 52-63 free -- used here for state that isn't otherwise determined by which cards
    * remain in each hand, but that still affects the game-theoretic value of the position:
    * the current trick's leader and in-progress plays (`trickPlays.size` is always 0..3;
    * see `play`), and `tricks.ns` (`tricks.ew` is derivable from remaining cards minus
    * `tricks.ns`, so doesn't need its own slot). Without these, two genuinely different
    * positions -- same remaining cards, but reached via a different trick history, e.g. a
    * different NS/EW split of already-completed tricks with the same next leader -- would
    * hash identically. See `State.evaluateKey` in the object-graph engine for the bug this
    * mirrors and avoids from the start.
    */
  def evaluateKey: CacheKey =
    def playBits(p: TrickPlay): Long = (p.suitIndex.toLong << 4) | p.rank.toLong
    def slotBits(index: Int): Long = if trickPlays.sizeIs > index then playBits(trickPlays(index)) else 0L

    val extra0 = (leader.toLong << 8) | (trickPlays.size.toLong << 6) | tricks.ns.toLong
    val extra1 = slotBits(0)
    val extra2 = slotBits(1)
    val extra3 = slotBits(2)

    (
      deal.hand(0).bits | (extra0 << 52),
      deal.hand(1).bits | (extra1 << 52),
      deal.hand(2).bits | (extra2 << 52),
      deal.hand(3).bits | (extra3 << 52)
    )
