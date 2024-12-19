/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.misc.{JPredicate, Predicate}

import scala.language.postfixOps
import scala.util._

object Checker {
  /**
    * JPredicate to test for a penalty in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Checker
    */
  def penaltyChecker(dir: Boolean): Checker = {
    // CONSIDER rewriting this with just one jLens (using a function to negate AND project. {
    val negative: String = s""
    val value: String = if (dir) "NS" else "EW"
    new JPredicate[SB] {
      def justification(sb: SB): Option[String] = penaltyIsOk(sb)
    }.jLens[SB](negative)(sb => sb.negate).jLens(value)(sv => sv.project(dir))
  }

  /**
    * JPredicate to test for a game in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Checker
    */
  def gameChecker(dir: Boolean, doubled: Boolean = false): Checker =
    gamePredicate(doubled).jLens(if (dir) "NS" else "EW")(u => u.project(dir))

  /**
    * JPredicate to test for a partial in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Checker
    */
  private def partialChecker(dir: Boolean, doubled: Boolean): Checker = {
    val bonus = if (doubled) 100 else 50
    val partial: String = s"partial"
    val value: String = s"""${if (dir) "NS" else "EW"}"""
    trickScorePredicate(doubled).jLens[SB](partial)(sb => sb.deduct(bonus)).jLens(value)(_.project(dir))
  }

  lazy val Partial: Checker = (for (x <- Seq(true, false); y <- Seq(false, true)) yield partialChecker(x, y)) reduce ((a, b) => a orElse b)
  lazy val Penalty: Checker = penaltyChecker(true) orElse penaltyChecker(false)
  lazy val Game: Checker = gameChecker(dir = true) orElse gameChecker(dir = false)
  private lazy val DoubledGame: Checker = gameChecker(dir = true, doubled = true) orElse gameChecker(dir = false, doubled = true)

  // NOTE: we accept doubled contracts as OK,
  // but anything redoubled or with doubled overtricks will need to be questioned.
  lazy val Valid: Checker = Game orElse Penalty orElse Partial orElse DoubledGame

  // Import Compound for :| method

  import Predicate.Compound

  private def suitPartialJ(strain: String, value: Int): JPredicate[Int] =
    (score: Int) => {
      val n = score / value
      Option.when(score :| value && Range(1, 8).contains(n))(s"$n $strain")
    }

  def stripBonus(bonusV: Int, bonusN: Int)(sv: SB): Int = sv.score - (if (sv.vulnerability) bonusV else bonusN)

  private lazy val minorPartial = suitPartialJ("minor", 20)
  private lazy val majorPartial = suitPartialJ("major", 30)

  private def notrumpPartial(doubled: Boolean) = new JPredicate[Int]() {
    def justification(score: Int): Option[String] = {
      if (doubled)
        if (score == 80) Some("1 NT") else suitPartialJ("NT", 60).justification(score - 20)
      else if (score == 40) Some("1 NT") else suitPartialJ("NT", 30).justification(score - 10)

    }
  }

  private lazy val doubledMinorPartial = suitPartialJ("Xminor", 40)
  private lazy val doubledMajorPartial = suitPartialJ("Xmajor", 60)

  // NOTE we don't accept doubled overtricks or redoubled contracts here--they must be questioned.

  /**
    * Determines the applicable `JPredicate[Int]` for evaluating trick scores,
    * based on whether the contract is doubled or not. The predicate combines different scoring conditions
    * (e.g., notrump, major suits, minor suits) through logical composition.
    *
    * @param doubled a Boolean value that specifies whether the trick score predicate
    *                should consider the scenario of a doubled contract. Defaults to false.
    * @return a `JPredicate[Int]` that evaluates trick scores according to the defined game rules.
    */
  def trickScorePredicate(doubled: Boolean = false): JPredicate[Int] =
    notrumpPartial(doubled) orElse
      (if (doubled) doubledMajorPartial orElse doubledMinorPartial else majorPartial orElse minorPartial)

  /**
    * Constructs a `JPredicate[SB]` that evaluates whether a given score and vulnerability (`SB`) satisfies
    * the conditions for various game, slam, and grand slam contracts, with specific bonus deductions
    * based on whether the contract is doubled.
    *
    * @param doubled a Boolean indicating whether the predicate evaluation should consider the condition
    *                for a doubled contract.
    * @return a `JPredicate[SB]` that evaluates the validity of game, slam, or grand slam conditions,
    *         applying respective bonus deductions.
    */
  private def gamePredicate(doubled: Boolean): JPredicate[SB] =
    trickScorePredicate(doubled).jLens("game")(stripBonus(500, 300)) orElse
      trickScorePredicate(doubled).jLens("slam")(stripBonus(1250, 800)) orElse
      trickScorePredicate(doubled).jLens("grand slam")(stripBonus(2000, 1300)) orElse {
      if (doubled)
        trickScorePredicate(true).jLens("gameX")(stripBonus(550, 350)) orElse
          trickScorePredicate(doubled).jLens("slam")(stripBonus(1300, 850)) orElse
          trickScorePredicate(doubled).jLens("grand slam")(stripBonus(2050, 1350))
      else
        JPredicate.never
    }

  // NOTE: we accept doubled contracts as OK, but anything redoubled needs to be questioned.

  /**
    * Predicate method of type `SB => Boolean`.
    * Recognizes penalties that are positive--i.e., from the point of view of the defenders.
    *
    * @param sb a `SB` value, i.e. score and vulnerability as a `Boolean`
    * @return `true` if the score matches a valid penalty: i.e., the first part of the tuple returned by `penaltyIsOkWithMaybeString`.
    */
  private def penaltyIsOk(sb: SB): Option[String] = penaltyIsOkWithMaybeString(sb)

  /**
    * Method to yield a `Boolean` and an optional `String` which is the explanation of the result (and is defined only if the `Boolean` value is `true`).
    *
    * NOTE that the second part of the tuple is always ignored for now. This method is for future use.
    *
    * @param sb a `SB` value, i.e. score and vulnerability as a `Boolean`
    * @return a tuple of `Boolean` and optional `String`.
    *         If the `Boolean` is `true` (the score matches a valid penalty),
    *         then the optional `String` will be defined as the reason for the penalty.
    */
  private def penaltyIsOkWithMaybeString(sb: SB): Option[String] =
    sb.score match {
      case 50 | 150 | 250 | 350 | 450 | 550 | 650 =>
        penalty(!sb.vulnerability, doubled = false, sb)
      case 700 | 900 | 1000 | 1200 | 1300 =>
        penalty(sb.vulnerability, doubled = false, sb)
      case 100 | 300 | 500 | 800 | 1100 | 1400 | 1700 | 2000 | 2300 | 2600 | 2900 | 3200 | 3500 if !sb.vulnerability =>
        penalty(matchVulnerability = true, doubled = true, sb)
      case 200 | 500 | 800 | 1100 | 1400 | 1700 | 2000 | 2300 | 2600 | 2900 | 3200 | 3500 | 3800 if sb.vulnerability =>
        penalty(matchVulnerability = true, doubled = true, sb)
      case 100 | 200 | 300 | 400 | 500 | 600 if !sb.vulnerability =>
        penalty(matchVulnerability = true, doubled = false, sb)
      case 100 | 200 | 300 | 400 | 500 | 600 =>
        penalty(sb.vulnerability, doubled = false, sb)
      case _ =>
        None
    }

  /**
    * Determines the penalty string for a given scenario based on match vulnerability, doubling, and score board (SB).
    *
    * NOTE If `matchVulnerability` is false, the result will always be None.
    *
    * @param matchVulnerability Specifies whether the match vulnerability condition is met (true) or not (false).
    * @param doubled            Indicates whether the contract is doubled (true) or not (false).
    * @param sb                 An instance of `SB` representing the score and vulnerability status.
    * @return An optional string representing the penalty if the conditions are met; otherwise, `None`.
    */
  private def penalty(matchVulnerability: Boolean, doubled: Boolean, sb: SB): Option[String] =
    Play.conditional(down(sb.vulnerability, doubled, sb.score))(matchVulnerability)

  private def down(vulnerable: Boolean, doubled: Boolean, score: Int): String = {
    val (undertricks, suffix) = (vulnerable, doubled) match {
      case (_, false) =>
        score / (if (vulnerable) 100 else 50) -> ""
      case (false, true) =>
        (score match {
          case 100 => 1
          case 300 => 2
          case 500 => 3
          case _ => 3 + (score - 500) / 300
        }) -> "X"
      case _ =>
        (score match {
          case 200 => 1
          case 500 => 2
          case _ => 2 + (score - 500) / 300
        }) -> "X"
    }
    s"down $undertricks$suffix"
  }

  def error(s: String): PlayResult = PlayResult(Left(s))
}

/**
  * Represents vulnerability in a game, modeled with two boolean flags.
  *
  * @param ns Indicates North-South vulnerability.
  * @param ew Indicates East-West vulnerability.
  */
case class Vulnerability(ns: Boolean, ew: Boolean) {
  def invert: Vulnerability = Vulnerability(!ns, !ew)
}

/**
  * Defines the vulnerability state in a game. It provides predefined
  * instances for specific vulnerability states (O, N, E, B) and a method
  * to compute the vulnerability based on a board number.
  *
  * Predefined states are:
  *   - `O`: Neither side is vulnerable.
  *   - `N`: North-South is vulnerable.
  *   - `E`: East-West is vulnerable.
  *   - `B`: Both sides are vulnerable.
  */
//noinspection NameBooleanParameters
object Vulnerability {
  val O: Vulnerability = Vulnerability(false, false)
  val N: Vulnerability = Vulnerability(true, false)
  val E: Vulnerability = Vulnerability(false, true)
  val B: Vulnerability = Vulnerability(true, true)

  /**
    * See https://tedmuller.us/Bridge/Esoterica/BoardVulnerability.htm
    *
    * @param x the board number.
    * @return
    */
  def apply(x: Int): Vulnerability = (x - 1) % 16 match {
    case 3 | 6 | 9 | 12 => B
    case 1 | 4 | 11 | 14 => N
    case 2 | 5 | 8 | 15 => E
    case 0 | 7 | 10 | 13 => O
    case _ => throw new Exception(s"logic error: $x")
  }
}

/**
  * Represents a scored result with an associated vulnerability.
  *
  * @param score         The integer value of the score.
  * @param vulnerability The vulnerability associated with the score.
  */
case class ScoreVul(score: Int, vulnerability: Vulnerability) {
  def project(direction: Boolean): SB = if (direction) SB(score, vulnerability.ns) else SB(-score, vulnerability.ew)
}

/**
  * Represents a score and its associated vulnerability status.
  *
  * CONSIDER do we really need this as a separate type from ScoreVul?
  *
  * @param score         The score value as an integer.
  * @param vulnerability Indicates if the instance is vulnerable (true) or not (false).
  */
case class SB(score: Int, vulnerability: Boolean) {
  def negate: SB = copy(score = -score)

  def deduct(bonus: Int): Int = score - bonus
}

