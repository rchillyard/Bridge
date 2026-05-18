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
    * Analyzes the potential outcome of a double-dummy play scenario based on the given parameters.
    *
    * @param tricks        The number of tricks needed by the protagonists to succeed.
    * @param directionNS   A boolean indicating whether the protagonists are North-South (true) or East-West (false).
    * @param depth         The depth of the search tree for the analysis. Defaults to the minimum of the cards in the deal or the maximum cards per deal.
    * @param reuseDeeper   A boolean indicating whether deeper search results can be reused during the analysis. Defaults to true.
    * @param depthTranches A boolean to determine if the depth of the search should use a tranches mechanism. Defaults to false.
    * @return An Option containing a boolean. Returns `Some(true)` if the analysis determines a winning outcome for the protagonists,
    *         `Some(false)` if losing, or `None` if no result is found.
    */
  def analyzeDoubleDummy(
                          tricks: Int,
                          directionNS: Boolean,
                          depth: Int = math.min(Deal.CardsPerDeal, deal.nCards),
                          reuseDeeper: Boolean = false,
                          depthTranches: Boolean = true
                        ): Option[Boolean] =
    val stateTC = WhistState(tricks, directionNS)
    val gameTC = WhistGame(this)
    given com.phasmidsoftware.gambit.game.State[State, State] = stateTC
    given com.phasmidsoftware.gambit.game.Game[State, CardPlay, Int] = gameTC

    deal.assertAdjusted()

    val player = AlphaBetaPlayer[State, State, CardPlay, Int](
      me = if directionNS then 0 else 1,
      depth = depth,
      keyFn = Some(_.evaluateKey),
      depthTranches = depthTranches,
      reuseDeeper = reuseDeeper,
      maxTableSize = Whist.MAX_STATES
    )
    val initialState = State(this)
    logger.info(s"analyzeDoubleDummy: neededTricks=$tricks, directionNS=$directionNS, depth=$depth, branching=${initialState.enumeratePlays.size}")
    if (depthTranches)
      logger.info(s"analyzeDoubleDummy: with depthTranches and reuseDeeper=$reuseDeeper}")
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
  val MAX_STATES = 800000
