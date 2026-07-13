/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.cards.Whist.{logger, runPlayer}
import com.phasmidsoftware.bridge.gambit.{WhistGame, WhistState}
import com.phasmidsoftware.bridge.pbn.{PBN, PBNParser}
import com.phasmidsoftware.gambit.game.{AlphaBetaPlayer, AlphaBetaWindow, FlatTTCache, TTCache}
import com.phasmidsoftware.gambit.util.LazyLogger

import scala.util.{Random, Success}

type BridgePlayer = AlphaBetaPlayer[State, State, CardPlay, Int, CacheKey]

/**
  * The result of a double-dummy analysis.
  *
  * - [[DDResult.Exact]]       — full search completed; result is definitive.
  * - [[DDResult.Partial]]     — node limit hit, but one side found a witness line;
  *   result is a qualified best-effort.
  * - [[DDResult.Inconclusive]] — node limit hit before either side found a witness;
  *   no reliable conclusion can be drawn.
  */
enum DDResult:
  case Exact(makes: Boolean, tricks: Int)
  case Partial(makes: Boolean, tricks: Int)
  case Inconclusive

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
  def play(cardPlay: CardPlay): Whist =
    Whist(deal.play(cardPlay), openingLeader, strain)

  /**
    * Analyzes the potential outcome of a double-dummy play scenario based on the given parameters.
    *
    * @param tricks      The number of tricks needed by the protagonists to succeed.
    * @param directionNS A boolean indicating whether the protagonists are North-South (true) or East-West (false).
    * @param depth       The depth of the search tree for the analysis. Defaults to the minimum of the cards in the deal or the maximum cards per deal.
    * @return A [[DDResult]]:
    *         - [[DDResult.Exact]] if the full search completed.
    *         - [[DDResult.Partial]] if the node limit was hit but one side found a witness line.
    *         - [[DDResult.Inconclusive]] if the node limit was hit before any witness was found.
    */
  def analyzeDoubleDummy(
                          tricks: Int,
                          directionNS: Boolean,
                          depth: Int = math.min(Deal.CardsPerDeal, deal.nCards)
                        ): DDResult =

    given gameTC: WhistGame = new WhistGame(this) // needs to be a named given so WhistState can find it

    given stateTC: WhistState = new WhistState(tricks, directionNS)
    given com.phasmidsoftware.gambit.game.State[State, State] = stateTC
    given com.phasmidsoftware.gambit.game.Game[State, CardPlay, Int] = gameTC

    given TTCache[CacheKey] = FlatTTCache(maxSize = BridgeConfig.ttMaxSize)

    deal.assertAdjusted()
    val player = new BridgePlayer(
      me = if directionNS then 0 else 1,
      depth = depth
    ).withMaxNodes(BridgeConfig.nodesPerIteration)
      .withKeyFn(s => s.evaluateKey)
      .withAspirationWindow(AlphaBetaWindow(-BridgeConfig.aspirationWindow, BridgeConfig.aspirationWindow))
    val initialState = State(this)
    logger.info(s"analyzeDoubleDummy: neededTricks=$tricks, directionNS=$directionNS, depth=$depth, branching=${initialState.enumeratePlays.size}")
    val t0 = System.currentTimeMillis()
    val result: DDResult = runPlayer(player, directionNS, depth, initialState, new Random(0L))
    logger.info(s"analyzeDoubleDummy: maxNSTricks=${stateTC.maxNSTricks}")
    logger.info(s"analyzeDoubleDummy: result=$result, elapsed=${System.currentTimeMillis() - t0}ms, tableSize=${player.tableSize}")
    result

  override def toString: String =
    s"Whist($deal, ${Hand.name(openingLeader)}, $sStrain)"

  /**
    * Method to enact the pending promotions on this Quittable.
    *
    * @return an eagerly promoted Whist game.
    */
  def quit: Whist =
    Whist(deal.quit, openingLeader, strain)

  /**
    * Create an initial state for this Whist game.
    * NOTE: only used in unit testing.
    */
  lazy val createState: State = State(this)

  private lazy val sStrain: String = strain.map(_.toString).getOrElse("NT")


object Whist:

  /**
    * Runs iterative deepening search and converts the result to a [[DDResult]].
    *
    * - Completes all iterations to `depth`: [[DDResult.Exact]] with tricks = depth / CardsPerTrick.
    * - Node limit fires after at least one completed iteration: [[DDResult.Partial]]
    *   with tricks = completedDepth / CardsPerTrick.
    * - Node limit fires before any iteration completes: [[DDResult.Inconclusive]].
    */
  private def runPlayer(player: BridgePlayer, directionNS: Boolean, depth: Int, initialState: State, random: Random) =
    player.chooseMoveIterativeDeepening(initialState, random, Whist.DEPTH_STEP) match
      case Some((_, score, completedDepth)) =>
        val makes = if directionNS then score > 0 else score < 0
        val tricksSearched = completedDepth / Deal.CardsPerTrick
        if completedDepth >= depth then DDResult.Exact(makes, tricksSearched)
        else DDResult.Partial(makes, tricksSearched)
      case None =>
        DDResult.Inconclusive

  val MAX_STATES: Int = 800_000
  val MAX_NODES: Int = 5_000_000 // retained for reference / future use
  val DEPTH_STEP: Int = Deal.CardsPerTrick // 4: iterate at trick boundaries -- a trick is always 4 cards,
  // not a tunable assumption, so unlike NODES_PER_ITERATION/aspiration-window this isn't in BridgeConfig.
  private val logger = LazyLogger(getClass)

@main def doubleDummySolver(args: String*): Unit = {
  import scala.io.{Codec, Source}
  assert(args.nonEmpty, "At least one argument required")
  val filename = args(0)
  val maybeBoard = args.lift(1).flatMap(_.toIntOption)

  given Codec = Codec.UTF8

  PBNParser.parsePBN(Source.fromFile(filename)) match {
    case Success(PBN(games)) =>
      (for (x <- maybeBoard; g <- games.lift(x)) yield g) match {
        case Some(g) =>
          g.analyzeMakableContracts()
        case None =>
          System.err.println(s"No game to parse PBN file: $filename ($maybeBoard)")
      }
    case _ =>
      System.err.println(s"Failed to parse PBN file: $filename")
      return
  }
}