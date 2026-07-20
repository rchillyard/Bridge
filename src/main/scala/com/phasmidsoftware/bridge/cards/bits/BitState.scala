/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import com.phasmidsoftware.bridge.cards.{BridgeConfig, CacheKey, Tricks}
import com.phasmidsoftware.gambit.util.LazyLogger

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
    * A candidate play paired with its already-computed move-ordering score. Two distinct
    * reasons this is a dedicated case class rather than a `(TrickPlay, Int)`/`(TrickPlay,
    * SuitMask)` tuple, found via profiling a real search:
    *
    *   - A generic tuple's fields box to `Object`/`Integer` regardless of their static type
    *     (`SuitMask` is an opaque `Int`, so a `(TrickPlay, SuitMask)` pair boxes its second
    *     field on every equivalence class, at every node in the whole search) -- the same
    *     class of cost `CacheKey` had before it became a dedicated case class.
    *   - `score` is computed once, here, rather than inside a `sortBy` comparator: `sortBy`
    *     doesn't cache its key function's result, so `sortBy(p => -expensiveScore(p))`
    *     recomputes `expensiveScore` on every pairwise comparison -- O(b log b) calls for
    *     `b` candidates, not O(b) -- the same latent cost found (and fixed, in Gambit's
    *     `orderedMoves`) while investigating a since-reverted, much more expensive
    *     experimental heuristic. `leadScore`/`followSuitScore`/`discardScore` are cheap
    *     enough that this was never the dominant cost, but there's no reason to pay it
    *     twice over (boxing AND recomputation) when avoiding both is this direct.
    */
  private case class ScoredPlay(play: TrickPlay, score: Int)

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

    def classesInSuit(suitIndex: Int, score: (TrickPlay, SuitMask) => Int): Seq[ScoredPlay] =
      deal.equivalenceClasses(player, suitIndex).map { cls =>
        val p = TrickPlay(player, suitIndex, cls.topRank)
        ScoredPlay(p, score(p, cls))
      }.toSeq

    val result =
      if trickPlays.isEmpty then
        (0 until 4).filter(s => deal.hand(player).suitMask(s).nonEmpty).flatMap(classesInSuit(_, leadScore))
          .sortBy(-_.score).map(_.play)
      else
        val suit = ledSuit
        if deal.hand(player).suitMask(suit).nonEmpty then
          classesInSuit(suit, followSuitScore).sortBy(-_.score).map(_.play)
        else
          (0 until 4).filterNot(_ == suit).filter(s => deal.hand(player).suitMask(s).nonEmpty)
            .flatMap(classesInSuit(_, (p, _) => discardScore(p))).sortBy(-_.score).map(_.play)
    // TRACE-only: the branching factor at this node -- how many candidate plays (equivalence-
    // class representatives, not raw cards) `legalPlays` returned here. Off by default (and
    // effectively free when off: LazyLogger guards on isTraceEnabled before building the
    // string); enable via logback for `com.phasmidsoftware.bridge.cards.bits.BitState` to get
    // a per-node branching-factor trace for diagnosing tree size.
    BitState.logger.trace(s"legalPlays: player=$player, tricksPlayed=${tricks.ns + tricks.ew}, trickPlays.size=${trickPlays.size}, branching=${result.size}")
    result

  /**
    * Move-ordering score for an opening lead of `p` from an equivalence class `cls`; higher
    * is tried first. A priority scale of tactical lead conventions, translated for
    * double-dummy analysis (where the whole partnership's actual holding is known, rather
    * than inferred from bidding or a partner's signal -- so a convention whose real-bridge
    * purpose is signalling, like fourth-best, doesn't obviously carry over; one whose
    * purpose is a genuine tactical property does):
    *
    *   1. Singleton lead in a plain (non-trump) suit, in a suit contract, PROVIDED this
    *      hand itself holds at least one trump -- with none, there's no ruff to set up.
    *      The bit engine's move-ordering counterpart to the object-graph engine's `Stiff`
    *      strategy (`Trick.leadStrategy`), which it never ported.
    *   2. Pseudo-sequence lead, plain suits only: my hand and partner's hand, combined,
    *      hold a run of equivalent cards spanning both hands -- e.g. my K plus partner's
    *      QJ. Lead the class of MY OWN cards below that combined run (the low spot cards,
    *      not the honor): classic "lead toward strength," hoping to trap a short holding
    *      of whatever's missing (the ace, here) in the seat playing right after this lead.
    *   3. The same idea, tolerating a gap (rule 5, "the basic idea of finessing"): the
    *      combined run may have a missing card, as long as it's held by the seat playing
    *      immediately after this lead (`(handIndex+1)%4` -- the seat a finesse plays
    *      through) rather than the seat playing after partner (`(handIndex+3)%4`, too late
    *      to be trapped this way). Implemented as the SAME combined-run computation as
    *      rule 2, just built with a narrower "opponent" mask (only the far seat's cards
    *      break the run -- the near seat's cards don't, the same way partner's never do).
    *   4. Trump lead: the two opponent hands' trump lengths are unequal -- call the longer
    *      one "declarer" and the shorter one "dummy," borrowing the usual bridge terms even
    *      though this model has no actual declarer/dummy seat -- AND dummy is shorter than
    *      declarer in some plain suit. That's the classic risk of the short-trump hand
    *      scoring extra tricks by ruffing the long-trump hand's losers in that suit; leading
    *      trumps first strips dummy's ruffing potential before it can be used.
    *
    *   A real fifth category -- doubleton leads, favorable when third hand holds two
    *   winners in the suit, or one winner plus a trump winner with 2+ trumps -- is
    *   deliberately NOT implemented yet. Successful doubleton leads are comparatively rare,
    *   and this is move-ordering only (it can never affect correctness, only search
    *   efficiency), so the gap is safe to leave open.
    *
    *   Falls back to the original suit-length + honor-sequence-bonus + rank scoring when
    *   none of the above apply.
    */
  private def leadScore(p: TrickPlay, cls: SuitMask): Int =
    val me = p.handIndex
    val suit = p.suitIndex
    val isPlainSuit = !strain.contains(suit)

    def isSingletonPlainSuitLead: Boolean =
      isPlainSuit && deal.hand(me).suitMask(suit).size == 1 &&
        strain.exists(t => deal.hand(me).suitMask(t).nonEmpty) // no ruff to set up without a trump myself

    // The partnership's combined run in `suit`, tolerating a gap held by the near seat
    // (rule 5): only the far seat's cards (`(me+3)%4`) break the run, the same way
    // `equivalenceClasses` already treats partner's cards as never breaking one.
    def combinedRuns: Array[SuitMask] =
      val sideBits = deal.sideMask(me, suit)
      val farSeatBits = deal.hand((me + 3) % 4).suitMask(suit)
      SuitMask.equivalenceClasses(sideBits, farSeatBits)

    def isPseudoSequenceLead: Boolean =
      isPlainSuit && {
        val myBits = deal.hand(me).suitMask(suit).bits
        combinedRuns.exists { run =>
          run.size >= 2 && (run.bits & myBits) != 0 && (run.bits & ~myBits) != 0 &&
            cls.topRank < Integer.numberOfTrailingZeros(run.bits) // cls lies entirely below the run
        }
      }

    def isTrumpLeadCandidate: Boolean =
      strain.contains(suit) && {
        val oppA = (me + 1) % 4
        val oppB = (me + 3) % 4
        val trumpA = deal.hand(oppA).suitMask(suit).size
        val trumpB = deal.hand(oppB).suitMask(suit).size
        trumpA != trumpB && {
          val (declarer, dummy) = if trumpA > trumpB then (oppA, oppB) else (oppB, oppA)
          (0 until 4).filterNot(_ == suit).exists { plainSuit =>
            deal.hand(dummy).suitMask(plainSuit).size < deal.hand(declarer).suitMask(plainSuit).size
          }
        }
      }

    if isSingletonPlainSuitLead then BitState.LeadSingletonPriority + p.rank
    else if isPseudoSequenceLead then BitState.LeadPseudoSequencePriority + p.rank
    else if isTrumpLeadCandidate then BitState.LeadTrumpPriority + p.rank
    else
      val suitLength = deal.hand(me).suitMask(suit).size
      val isRealSequence = cls.size >= 2
      val isHonor = p.rank >= SuitMask.RanksPerSuit - BitState.LeadFallbackHonorThreshold
      val sequenceBonus = if isRealSequence && isHonor then BitState.LeadFallbackSequenceBonus else 0
      suitLength * BitState.LeadFallbackSuitLengthWeight + sequenceBonus + p.rank

  /**
    * Move-ordering score for following suit; higher is tried first. Ported from
    * `Holding.getStrategyForFollowingSuit`/`applyFollowSuitStrategy`, distinguishing
    * trick position (`trickPlays.size`, i.e. how many cards are already down) the same
    * way the old engine's `Strategy` selection does:
    *
    *   - 2nd hand (`size == 1`): only even considers trying to win if the led card is an
    *     honor or this hand holds a genuine (2+ card) sequence of its own -- `Cover` in
    *     the old engine's terms. Otherwise (`Duck`): always play low, even if a cheap card
    *     could technically beat the led one (classic "second hand low").
    *   - 3rd hand (`size == 2`): if partner's already winning, don't bother (`Duck`). Else,
    *     if the card to beat isn't an honor, play to win outright with the highest
    *     available card (`WinIt`) rather than finesse; if it is an honor, finesse --
    *     cover as cheaply as possible (`Finesse`, same formula as `Cover`).
    *   - 4th/last hand (`size == 3`): always `Cover` -- cheapest sufficient card if one
    *     exists, else low (nothing left to preserve by this point anyway).
    *
    * In every case where winning is "considered" and this candidate can't actually beat
    * the provisional winner, the score falls back to preferring the lowest card, matching
    * the old engine's behaviour of never distinguishing "won't try" from "can't beat it
    * anyway" once a strategy has decided not to press for the win.
    */
  private def followSuitScore(p: TrickPlay, cls: SuitMask): Int =
    val winner = provisionalWinner.get // always defined: trickPlays.nonEmpty and p follows ledSuit
    val partnerWinning = winner.handIndex % 2 == p.handIndex % 2
    val canBeat = p.rank > winner.rank
    val winnerIsHonor = winner.rank >= SuitMask.RanksPerSuit - 5

    // considerWinning/preferHighWin computed as two separate values, not a tuple: a
    // (Boolean, Boolean) pair here would allocate fresh on every call, at every node --
    // found via profiling (see legalPlays' ScoredPlay doc for the same lesson elsewhere).
    val considerWinning = trickPlays.size match
      case 1 => // 2nd hand: Cover only if the led card is an honor or we hold a real sequence
        val ledIsHonor = trickPlays.head.rank >= SuitMask.RanksPerSuit - 5
        ledIsHonor || cls.size >= 2
      case 2 => !partnerWinning // 3rd hand: Duck if partner's winning
      case _ => true // 4th/last hand (size == 3): always Cover
    val preferHighWin = trickPlays.size == 2 && !partnerWinning && !winnerIsHonor // 3rd hand, WinIt case

    if canBeat && considerWinning then
      if preferHighWin then p.rank else winner.rank - p.rank // WinIt: highest; Cover/Finesse: cheapest sufficient
    else -p.rank // duck, or can't usefully win: prefer the lowest card

  /**
    * Move-ordering score for a discard or ruff; higher is tried first. Ported from
    * `Hand.discardOrRuff`: prefer ruffing over discarding, unless partner already holds the
    * trick (matching the old engine's `Ruff if isPartnerWinning => redirect(Discard)`).
    * Within ruffs, prefer the lowest trump -- but, unlike the old engine, only as an
    * ordering preference: see this method's class doc for why the old engine's harder
    * restriction (try ONLY the lowest card per suit) was deliberately not carried over.
    *
    * Within discards (never applies to ruffs, where there's only one trump suit to choose
    * from), also prefers a SUIT to discard from, not just a rank: "keep length with
    * dummy/declarer" is a real defensive principle the old engine's `Strategy` system never
    * modelled either (`Hand.discardOrRuff` only ever compares candidates by rank, same gap
    * this port started with). The proxy here is coarse -- how much longer this hand is than
    * the OTHER partnership (`DealBits.opponentMask`) in each candidate suit -- but captures
    * the basic idea: discarding from a suit where this hand still has slack over the
    * declaring side's length there is safer than discarding from one where it doesn't.
    */
  private def discardScore(p: TrickPlay): Int =
    val isRuff = strain.contains(p.suitIndex)
    val partnerWinning = provisionalWinner.exists(w => w.handIndex % 2 == p.handIndex % 2)
    val ruffBonus = if isRuff && !partnerWinning then 1000 else 0
    val lengthSafety =
      if isRuff then 0
      else
        val ourLength = deal.hand(p.handIndex).suitMask(p.suitIndex).size
        val theirLength = deal.opponentMask(p.handIndex, p.suitIndex).size
        (ourLength - theirLength) * 5
    ruffBonus + lengthSafety - p.rank

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
    * A transposition-table key. Each hand's `HandBits` already uses only bits
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

  /**
    * Rank-reduced transposition-table key: an EXPERIMENTAL, opt-in alternative to
    * [[evaluateKey]] (see `BitAnalysis`'s `useCanonicalKey` parameter), not yet the
    * default. Each suit's four hand-masks are compacted (`SuitMask.compact`/
    * `DealBits.canonicalSuitMasks`) to preserve only the RELATIVE order of that suit's
    * currently-live cards, not their absolute rank -- so two positions with the same
    * shape but different absolute cards can share a transposition-table entry, the same
    * "rank reduction" real double-dummy solvers use.
    *
    * Only used BETWEEN TRICKS (`trickPlays.isEmpty`); falls back to the plain
    * [[evaluateKey]] whenever a trick is in progress. This restriction is load-bearing,
    * not a simplification to relax later without re-deriving the argument below:
    *
    * `followSuitScore` (and the search generally) compares a live candidate's rank
    * against the trick's provisional winner's rank -- an ALREADY-PLAYED card -- via
    * `p.rank > winner.rank`, in the same absolute rank space. If only the *live* cards
    * were compacted while an in-progress play's rank stayed absolute, two genuinely
    * different positions could collide: e.g. a lead at absolute rank 6, with exactly one
    * remaining live card in that suit -- at absolute rank 10 in position S1 (beats the
    * lead) vs. rank 3 in position S2 (doesn't). Both compact to "one live card, canonical
    * position 0," so with the trick-slot rank left at the identical absolute value (6) in
    * both, the two keys would match -- a false merge, the same class of bug `evaluateKey`
    * itself was fixed twice for (see this class's doc). Restricting to between-tricks
    * removes the bug class entirely: at that point there is no absolute already-played
    * rank in the current trick for the compacted cards to be inconsistent with -- the
    * only other state (`leader`, `tricks.ns`) is preserved exactly as `evaluateKey`
    * already does.
    */
  def evaluateCanonicalKey: CacheKey =
    if trickPlays.nonEmpty then evaluateKey
    else
      val extra0 = (leader.toLong << 8) | tricks.ns.toLong // trickPlays.size and slot bits are always 0 here
      // Computed once per suit (4 suits total), not once per hand: each hand's canonical
      // mask for a suit depends on that suit's whole live-card universe, shared work.
      val canonicalPerSuit: IndexedSeq[IndexedSeq[SuitMask]] = (0 until 4).map(deal.canonicalSuitMasks)
      def canonicalHandBits(handIndex: Int): Long =
        (0 until 4).foldLeft(0L) { (acc, suitIndex) =>
          acc | (canonicalPerSuit(suitIndex)(handIndex).bits.toLong << (suitIndex * SuitMask.RanksPerSuit))
        }

      CacheKey(
        canonicalHandBits(0) | (extra0 << 52),
        canonicalHandBits(1),
        canonicalHandBits(2),
        canonicalHandBits(3)
      )

object BitState:
  private val logger = LazyLogger(getClass)

  /**
    * The opening-lead priority scale (`leadScore`), gathered here so the relative weight of
    * every consideration can be read off in one place rather than hunting through the method
    * body. Each named tactical-rule priority outranks every lower one unconditionally, then
    * falls back to the ordinary suit-length/sequence scoring below them all -- spaced 1000
    * apart, comfortably wider than both the 0..12 rank tie-break added within a band and the
    * fallback band's own worst case (`13 * LeadFallbackSuitLengthWeight +
    * LeadFallbackSequenceBonus + 12` = 242), so bands can never bleed into each other.
    */
  private val LeadSingletonPriority = 5000 // rule 1: singleton lead, suit contracts only
  private val LeadPseudoSequencePriority = 4000 // rules 2/3: lead toward a partnership tenace
  private val LeadTrumpPriority = 3000 // rule 4: strip the short-trump hand's ruffing potential
  private val LeadFallbackSuitLengthWeight = 10 // fallback: prefer a longer suit
  private val LeadFallbackSequenceBonus = 100 // fallback: prefer topping a genuine own sequence
  private val LeadFallbackHonorThreshold = 5 // fallback: top 5 ranks count as an honor (T,J,Q,K,A)
