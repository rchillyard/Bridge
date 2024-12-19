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
  def penaltyP(dir: Boolean): Checker = {
    // CONSIDER rewriting this with just one jLens (using a function to negate AND project. {
    val negative: SB => String = _ => s""
    val value: ScoreVul => String = _ => if (dir) "NS" else "EW"
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
  def gameP(dir: Boolean): Checker = {
    val game: ScoreVul => String = _ => if (dir) "NS" else "EW"
    gameP.jLens(game)(u => u.project(dir))
  }

  /**
    * JPredicate to test for a partial in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Checker
    */
  private def partialP(dir: Boolean, doubled: Boolean): Checker = {
    val bonus = if (doubled) 100 else 50
    val partial: SB => String = _ => s"partial"
    val value: ScoreVul => String = _ => s"""${if (dir) "NS" else "EW"}"""
    trickScorePredicate.jLens[SB](partial)(sb => sb.deduct(bonus)).jLens(value)(_.project(dir))
  }

  lazy val Partial: Checker = (for (x <- Seq(true, false); y <- Seq(false, true)) yield partialP(x, y)) reduce ((a, b) => a orElse b)
  lazy val Penalty: Checker = penaltyP(true) orElse penaltyP(false)
  lazy val Game: Checker = gameP(true) orElse gameP(false)
  lazy val Valid: Checker = Game orElse Penalty orElse Partial

  import Predicate._

  private def suitPartialJ(strain: String, value: Int): JPredicate[Int] =
    (score: Int) => {
      val n = score / value
      Option.when(score :| value && Range(1, 8).contains(n))(s"$n $strain")
    }

  def stripBonus(bonusV: Int, bonusN: Int)(sv: SB): Int = sv.score - (if (sv.vulnerability) bonusV else bonusN)

  private lazy val minorPartial = suitPartialJ("minor", 20)
  private lazy val majorPartial = suitPartialJ("major", 30)
  private lazy val notrumpPartial = new JPredicate[Int]() {
    def justification(score: Int): Option[String] =
      if (score == 40) Some("1 NT") else suitPartialJ("NT", 30).justification(score - 10)
  }

  private lazy val doubledMinorPartial = suitPartialJ("Xminor", 40)
  private lazy val doubledMajorPartial = suitPartialJ("Xmajor", 60)
  private lazy val doubledNotrumpPartial = doubledMajorPartial.lens[Int](_ - 20)

  // NOTE we don't accept doubled overtricks or redoubled contracts here--they must be questioned.
  lazy val trickScorePredicate: JPredicate[Int] = notrumpPartial orElse majorPartial orElse minorPartial orElse doubledMinorPartial orElse doubledMajorPartial orElse doubledNotrumpPartial
  private lazy val gameP: JPredicate[SB] =
    trickScorePredicate.jLens[SB](_ => "game")(stripBonus(500, 300)) orElse
    trickScorePredicate.jLens[SB](_ => "slam")(stripBonus(1250, 800)) orElse
    trickScorePredicate.jLens[SB](_ => "grand slam")(stripBonus(2000, 1300))

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
      case 100 | 200 | 300 | 400 | 500 | 600 | 800 | 1100 | 1400 | 1700 | 2000 | 2300 | 2600 | 2900 | 3200 | 3500 | 3800 =>
        penalty(sb.vulnerability, doubled = false, sb) // NOTE: unreachable?
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
        val perTrick = if (vulnerable) 100 else 50
        score / perTrick -> ""
      case (false, true) =>
        (score match {
          case 100 => 1
          case 300 => 2
          case 500 => 3
          case _ =>
            3 + (score - 500) / 300
        }) -> "X"
      case _ =>
        (score match {
          case 200 => 1
          case 500 => 2
          case _ =>
            2 + (score - 500) / 300
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

