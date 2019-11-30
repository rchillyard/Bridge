/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import scala.language.implicitConversions

/**
  * Trait to model the behavior of play-choosing strategy.
  * We aim to choose the most favorable play each time so that we can achieve our goal quicker.
  *
  * In general, we check these values in the same sequence as they are defined below.
  */
sealed trait Strategy {
  /**
    * @return true if the card played depends on its whether we can beat the current winner;
    *         false if we always play the same card.
    */
  val conditional: Boolean

  /**
    * @return true if we want to try to win the trick if possible.
    *         false if we are OK with not winning the trick.
    */
  val win: Boolean

  /**
    * @return an indication of the level of aggression of this Strategy (particularly related to opening leads).
    */
  val aggression: Int
}

abstract class BaseStrategy(val win: Boolean, val conditional: Boolean, val aggression: Int) extends Strategy

case object StandardOpeningLead extends BaseStrategy(false, false, 2)

case object WinIt extends BaseStrategy(true, false, 3)

case object LeadTopOfSequence extends BaseStrategy(true, false, 4)

case object LeadSecond extends BaseStrategy(true, false, 0)

case object Cover extends BaseStrategy(false, true, 2)

case object Finesse extends BaseStrategy(true, true, 3)

case object FourthBest extends BaseStrategy(false, false, 2)

case object Duck extends BaseStrategy(false, false, 0)

case object Discard extends BaseStrategy(false, false, 0)

case object Ruff extends BaseStrategy(false, false, 4)

case object Stiff extends BaseStrategy(false, false, 4)

case object Invalid extends BaseStrategy(false, false, 0)
