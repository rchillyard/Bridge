/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.util._

import scala.language.postfixOps

/**
  * This class represents the current trick totals for the two partnerships.
  *
  * @param ns the number of tricks NS has taken.
  * @param ew the number of tricks EW has taken.
  */
case class Tricks(ns: Int, ew: Int) extends Evaluatable {
  /**
    * Increment the total according to the value of winner.
    * CONSIDER using Hand.sameSide
    *
    * @param winner the index of the winner of a trick.
    * @return a new version of Tricks.
    */
  def increment(winner: Int): Tricks = if (winner % 2 == 0) incNS else incEW

  /**
    * Increment the total according to the play of the trick given.
    * Note that, increment(Int) is called only if the trick is complete.
    * Otherwise, this is returned.
    *
    * @param trick the trick.
    * @return a new version of Tricks.
    */
  def increment(trick: Trick): Tricks = trick.winner match {
    case Some(Winner(p, true)) => increment(p.hand)
    case _ => this
  }

  /**
    * Project this Tricks object according to a particular direction.
    *
    * @param directionNS true if we want this as is, else false in which case the trick totals are reversed.
    * @return an instance of Tricks.
    */
  def project(directionNS: Boolean): Tricks = if (directionNS) this else Tricks(ew, ns)

  /**
    * NOT USED
    *
    * Method to determine if the required number of tricks for the given direction have been acquired.
    *
    * @param tricks      the required number of tricks.
    * @param directionNS true if we are requiring NS tricks, otherwise false.
    * @return Some(Boolean) if a decision has been reached (true if the goal has been reached, else false);
    *         otherwise, None is returned.
    */
  def decide(tricks: Int, directionNS: Boolean): Option[Boolean] = project(directionNS).decide(tricks)

  /**
    * Method to determine if the required number of tricks for the given direction have been acquired.
    *
    * @param tricks the required number of tricks.
    * @return Some(Boolean) if a decision has been reached (true if the goal has been reached, else false);
    *         otherwise, None is returned.
    */
  def decide(tricks: Int): Option[Boolean] = if (goal(tricks)) Some(true)
  else if (counterGoal(tricks)) Some(false)
  else None

  /**
    * Method to determine if NS has achieved a given number of tricks.
    *
    * @param tricks the number of NS tricks required.
    * @return true if NS has at least tricks tricks.
    */
  def goal(tricks: Int): Boolean = ns >= tricks

  /**
    * Method to determine if EW has achieved a given number of tricks that makes the given target of tricks impossible.
    *
    * @param tricks the number of NS tricks required.
    * @return true if EW has at least 14-tricks tricks.
    */
  def counterGoal(tricks: Int): Boolean = ew >= Deal.TricksPerDeal + 1 - tricks

  /**
    * TODO test this.
    *
    * @return a Double representing the value of this Tricks.
    */
  def evaluate: Double = _evaluate

  lazy val incNS: Tricks = Tricks(ns + 1, ew)

  lazy val incEW: Tricks = Tricks(ns, ew + 1)

  override def toString: String = s"$ns:$ew"

  private lazy val _evaluate = ns - ew
}

object Tricks {
  val zero = Tricks(0, 0)

  implicit object LoggableTricks extends Loggable[Tricks] with Loggables {
    val loggable: Loggable[Tricks] = toLog2(Tricks.apply, List("ns", "ew"))

    def toLog(t: Tricks): String = s"${t.ns}:${t.ew}"
  }

}
