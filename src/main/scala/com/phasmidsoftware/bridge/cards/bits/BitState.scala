/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import com.phasmidsoftware.bridge.cards.{BridgeConfig, CacheKey, Tricks}

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
  *   - `legalPlays` now ports the object-graph engine's `Strategy` system
  *     (`Cover`/`Duck`/`Finesse`/`Ruff`/`Discard`/etc. in `Holding`/`Trick`) as a move-ordering
  *     score (`leadScore`/`followSuitScore`/`discardScore`, see `legalPlays`' doc), feeding
  *     Gambit's `AlphaBetaPlayer.orderedMoves` a pre-sorted candidate list instead of an
  *     arbitrary one. Deliberately NOT ported: the old engine's discard/ruff behaviour
  *     actually prunes to a single "always discard/ruff the worst card" candidate, not just
  *     reorders. This engine still offers EVERY equivalence-class representative in every
  *     legal suit for a discard/ruff (just ordered worst-first) -- strictly more (never
  *     fewer) legal candidates than the old engine tries, correct by construction.
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
    *
    * The three branches below are also the whole legal move-ordering story: within
    * whichever one applies (leading, following suit, or discarding/ruffing -- these are
    * mutually exclusive by construction, never mixed in one call), candidates are sorted
    * by [[leadScore]]/[[followSuitScore]]/[[discardScore]], a bit-native port of the
    * object-graph engine's `Strategy` system (`Cover`/`Duck`/`Finesse`/`LeadTopOfSequence`/
    * etc. in `Holding`/`Trick`). Every one of those scores is ORDERING ONLY: the full set
    * of equivalence-class representatives is still returned, just reordered, preserving
    * the "strictly more (never fewer) legal candidates than the old engine tries" property
    * documented above -- including for discard/ruff, where the old engine actually
    * restricts to a single (lowest) candidate per suit rather than merely reordering.
    * That restriction was deliberately not ported: it would be the first place in this
    * engine where "usually right" gets treated as "always assumed," a tradeoff already
    * discussed and declined in favour of ordering-only speedups (see the design doc's
    * "On Branching" section).
    */
  def legalPlays: Seq[TrickPlay] =
    val player = currentPlayer

    def classesInSuit(suitIndex: Int): Seq[(TrickPlay, SuitMask)] =
      deal.equivalenceClasses(player, suitIndex).map(cls => TrickPlay(player, suitIndex, cls.topRank) -> cls)

    if trickPlays.isEmpty then
      (0 until 4).filter(s => deal.hand(player).suitMask(s).nonEmpty).flatMap(classesInSuit)
        .sortBy((p, cls) => -leadScore(p, cls)).map(_._1)
    else
      val suit = ledSuit
      if deal.hand(player).suitMask(suit).nonEmpty then
        classesInSuit(suit).sortBy((p, _) => -followSuitScore(p)).map(_._1)
      else
        (0 until 4).filterNot(_ == suit).filter(s => deal.hand(player).suitMask(s).nonEmpty).flatMap(classesInSuit)
          .sortBy((p, _) => -discardScore(p)).map(_._1)

  /**
    * Move-ordering score for an opening lead of `p` from an equivalence class `cls`; higher
    * is tried first. Ported from `Trick.leadStrategy`/`Holding.getStrategyForFollowingSuit`'s
    * `StandardOpeningLead`/`LeadTopOfSequence` case and `chooseLeads`' longest-suit-first sort:
    * prefer a longer suit, and within a suit prefer leading a genuine (2+ card) sequence
    * topped by an honor over a lone card. `cls.topRank` (this candidate's rank) is already
    * the top of its run by construction (see `legalPlays`' class doc), so "lead top of
    * sequence" falls out for free once a suit is chosen -- this only affects suit choice.
    */
  private def leadScore(p: TrickPlay, cls: SuitMask): Int =
    val suitLength = deal.hand(p.handIndex).suitMask(p.suitIndex).size
    val isRealSequence = cls.size >= 2
    val isHonor = p.rank >= SuitMask.RanksPerSuit - 5 // top 5 ranks: T, J, Q, K, A
    val sequenceBonus = if isRealSequence && isHonor then 100 else 0
    suitLength * 10 + sequenceBonus + p.rank

  /**
    * Move-ordering score for following suit; higher is tried first. Ported from
    * `Holding.applyFollowSuitStrategy`: if this card beats the provisional winner and that
    * winner is an opponent's, prefer covering as cheaply as possible (the smallest winning
    * margin); otherwise -- can't beat it, or the provisional winner is already partner's --
    * prefer the lowest card, matching the old engine's `Duck` fallback in both cases.
    * (Simplified versus the old engine's full trick-position-dependent Cover/Finesse/WinIt
    * selection: this doesn't distinguish 2nd/3rd/4th hand, just "can I usefully beat the
    * current winner." A coarser heuristic than the original, but still ordering-only.)
    */
  private def followSuitScore(p: TrickPlay): Int =
    val winner = provisionalWinner.get // always defined: trickPlays.nonEmpty and p follows ledSuit
    val partnerWinning = winner.handIndex % 2 == p.handIndex % 2
    val canBeat = p.rank > winner.rank
    if canBeat && !partnerWinning then winner.rank - p.rank // cover as cheaply as possible
    else -p.rank // duck: can't usefully win, so prefer the lowest card

  /**
    * Move-ordering score for a discard or ruff; higher is tried first. Ported from
    * `Hand.discardOrRuff`: prefer ruffing over discarding, unless partner already holds the
    * trick (matching the old engine's `Ruff if isPartnerWinning => redirect(Discard)`), and
    * within either category prefer the lowest card -- but, unlike the old engine, only as an
    * ordering preference: see this method's class doc for why the old engine's harder
    * restriction (try ONLY the lowest card per suit) was deliberately not carried over.
    */
  private def discardScore(p: TrickPlay): Int =
    val isRuff = strain.contains(p.suitIndex)
    val partnerWinning = provisionalWinner.exists(w => w.handIndex % 2 == p.handIndex % 2)
    val ruffBonus = if isRuff && !partnerWinning then 1000 else 0
    ruffBonus - p.rank

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
    * NS-centric heuristic for non-terminal positions: tricks banked so far, scaled by
    * `BridgeConfig.heuristicScale` to stay safely inside the aspiration window
    * (`BridgeConfig.aspirationWindow`, which `BitAnalysis` also uses) -- that window's
    * search assumes every non-terminal value stays strictly inside it, with only a PROVEN
    * `isGoal` result (`heuristic` above returns `±Double.MaxValue` for those, overriding
    * this) allowed to fall outside. An unscaled integer trick-count would blow past the
    * window as soon as either side banks even one more trick than the other, causing the
    * narrow-window search to clamp/cut off before reaching a real conclusion.
    * `heuristicScale` is derived from `aspirationWindow`, not independently configured, so
    * the two can't drift out of the relationship that makes this safe -- see
    * `BridgeConfig`'s doc comments.
    */
  def heuristic: Double = (tricks.ns - tricks.ew).toDouble * BridgeConfig.heuristicScale

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

    CacheKey(
      deal.hand(0).bits | (extra0 << 52),
      deal.hand(1).bits | (extra1 << 52),
      deal.hand(2).bits | (extra2 << 52),
      deal.hand(3).bits | (extra3 << 52)
    )
