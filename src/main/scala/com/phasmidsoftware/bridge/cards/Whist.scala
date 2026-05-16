/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.gambit.{WhistGame, WhistState}
import com.phasmidsoftware.gambit.game.AlphaBetaPlayer

import scala.util.Random

/**
  * This class represents a game of Whist.
  * In Whist, there are four players around a table.
  * Players sitting opposite each other are part of the same "team." They are said to be partners.
  * Play goes clockwise around the table, each player contributing one card, and the team which
  * contributed the highest card (or possibly the highest trump) is credited with that "trick".
  * There being 52 cards in a deck (pack), there will be 13 tricks.
  * The player (not the team) who wins one trick must lead to the following trick.
  * At the start of the game, it is arbitrary which player is the opening leader — here it is
  * simply determined by parameter.
  *
  * The particular arrangement (shuffle) of the cards is determined by the deal parameter.
  *
  * @param deal          the arrangement of cards.
  * @param openingLeader the player on opening lead (0 thru 3 for "North" thru "West").
  * @param strain        the (optional) trump suit: None indicates notrump.
  */
case class Whist(deal: Deal, openingLeader: Int, strain: Option[Suit] = None)
  extends Playable[Whist] with Quittable[Whist]:

  /**
    * Method to make a sequence of States from the given sequence of Trick instances.
    *
    * @param tricks the current value of Tricks (i.e., current score NS vs. EW).
    * @param ts     a sequence of Trick instances.
    * @return a sequence of State objects corresponding to the values of ts.
    */
  def makeStates(tricks: Tricks, ts: Seq[Trick]): Seq[State] =
    ts.map(t => State.create(this, t, tricks))

  /**
    * Play a card from this Playable object.
    *
    * @param cardPlay the card play.
    * @return a new Whist.
    */
  def play(cardPlay: CardPlay): Whist = Whist(deal.play(cardPlay), openingLeader, strain)

  /**
    * Solve this Whist game as a double-dummy problem using alpha-beta search.
    *
    * NS (or EW, depending on `directionNS`) attempts to reach `tricks` tricks.
    * Uses `WhistState.isGoal` for early termination when the goal is achieved or
    * shown to be impossible (`goalImpossible` pruning).
    *
    * @param tricks      the number of tricks required by the protagonists.
    * @param directionNS if true, NS are the protagonists; otherwise EW.
    * @param depth       the alpha-beta search depth in plies (default: full game = 52).
    * @return `Some(true)` if the protagonists can achieve their goal,
    *         `Some(false)` if they cannot,
    *         `None` if no move was available (terminal position on entry).
    */
  def analyzeDoubleDummy(
                          tricks: Int,
                          directionNS: Boolean,
                          depth: Int = math.min(Deal.CardsPerDeal, deal.nCards)
                        ): Option[Boolean] =
    val stateTC = WhistState(tricks, directionNS)
    val gameTC = WhistGame(this)
    given com.phasmidsoftware.gambit.game.State[State, State] = stateTC
    given com.phasmidsoftware.gambit.game.Game[State, CardPlay, Int] = gameTC

    assert(deal.isAdjusted, "Deal must be adjusted before double-dummy analysis")

    val player = AlphaBetaPlayer[State, State, CardPlay, Int](
      me = if directionNS then 0 else 1,
      depth = depth,
      keyFn = Some(_.evaluateKey)
    )

    val initialState = State(this)
    logger.info(s"analyzeDoubleDummy: neededTricks=$tricks, directionNS=$directionNS, depth=$depth, branching=${initialState.enumeratePlays.size}")
    val t0 = System.currentTimeMillis()
    val result = player.chooseMove(initialState, new Random(0L)).map { cardPlay =>
      val bestSuccessor = gameTC.applyMove(initialState, cardPlay, openingLeader)
      stateTC.heuristic(bestSuccessor) > 0
    }
    logger.info(s"analyzeDoubleDummy: result=$result, elapsed=${System.currentTimeMillis() - t0}ms, tableSize=${player.tableSize}")
    result

  override def toString: String =
    s"Whist($deal, ${Hand.name(openingLeader)}, $sStrain)"

  /**
    * Method to enact the pending promotions on this Quittable.
    *
    * @return an eagerly promoted Whist game.
    */
  def quit: Whist = Whist(deal.quit, openingLeader, strain)

  /**
    * Create an initial state for this Whist game.
    * NOTE: only used in unit testing.
    */
  lazy val createState: State = State(this)

  private lazy val sStrain: String = strain.map(_.toString).getOrElse("NT")

  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)


object Whist:
  val MAX_STATES = 1000000