/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.cards.Whist.{logger, runPlayer}
import com.phasmidsoftware.bridge.gambit.{WhistGame, WhistState}
import com.phasmidsoftware.bridge.pbn.{PBN, PBNParser}
import com.phasmidsoftware.gambit.game.{AlphaBetaPlayer, FlatTTCache, TTCache}
import com.phasmidsoftware.gambit.util.{LazyLogger, Output, Outputable, Shuffle}

import scala.util.{Random, Success}

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
    * @param tricks        The number of tricks needed by the protagonists to succeed.
    * @param directionNS   A boolean indicating whether the protagonists are North-South (true) or East-West (false).
    * @param depth         The depth of the search tree for the analysis. Defaults to the minimum of the cards in the deal or the maximum cards per deal.
    * @return An Option containing a boolean. Returns `Some(true)` if the analysis determines a winning outcome for the protagonists,
    *         `Some(false)` if losing, or `None` if no result is found.
    */
  def analyzeDoubleDummy(
                          tricks: Int,
                          directionNS: Boolean,
                          depth: Int = math.min(Deal.CardsPerDeal, deal.nCards)
                        ): Option[Boolean] =

    given gameTC: WhistGame = new WhistGame(this) // needs to be a named given so WhistState can find it

    given stateTC: WhistState = new WhistState(tricks, directionNS)
    given com.phasmidsoftware.gambit.game.State[State, State] = stateTC
    given com.phasmidsoftware.gambit.game.Game[State, CardPlay, Int] = gameTC

    given TTCache[CacheKey] = FlatTTCache()

    deal.assertAdjusted()
    val player = new AlphaBetaPlayer[State, State, CardPlay, Int, CacheKey](
      me = if directionNS then 0 else 1,
      depth = depth,
      keyFn = Some(s => s.evaluateKey)
    )
    val initialState = State(this)
    logger.info(s"analyzeDoubleDummy: neededTricks=$tricks, directionNS=$directionNS, depth=$depth, branching=${initialState.enumeratePlays.size}")
    val t0 = System.currentTimeMillis()
    val result = runPlayer(player, initialState, new Random(0L), depth).map {
      (_, score) => score > 0
    }
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

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.*
  import scala.concurrent.{Await, Future, TimeoutException}
  import scala.util.{Failure, Try}

  private def runPlayer(player: AlphaBetaPlayer[State, State, CardPlay, Int, (Long, Long, Long, Long)], initialState: State, random: Random, depth: Int): Option[(CardPlay, Double)] = {
    val future: Future[Option[(CardPlay, Double)]] = Future(player.chooseMoveWithScore(initialState, random))
    Try(Await.result(future, 10.seconds)).recoverWith {
      case _: TimeoutException =>
        logger.warn(s"analyzeDoubleDummy: timed out after 10s, depth=$depth")
        Failure(new TimeoutException(s"analyzeDoubleDummy timed out"))
    }.toOption.flatten
  }


  val MAX_STATES = 800000
  private val logger = LazyLogger(getClass)

@main def myApp(args: String*): Unit = {
  import scala.io.{Codec, Source}
  assert(args.nonEmpty, "At least one argument required")
  val filename = args(0)
  val maybeBoard = args.lift(1).flatMap(_.toIntOption)

  given Codec = Codec.UTF8

  PBNParser.parsePBN(Source.fromFile(filename)) match {
    case Success(PBN(games)) =>
      (for (x <- maybeBoard; g <- games.lift(x)) yield g) match {
        case Some(g) =>
          g.analyzeMakableContracts(5) // NOTE this restricts the number contracts analyzed to 5
        case None =>
          System.err.println(s"No game to parse PBN file: $filename ($maybeBoard)")
      }
    case _ =>
      System.err.println(s"Failed to parse PBN file: $filename")
      return
  }
}