/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.flog.{Flog, Loggable}
import com.phasmidsoftware.misc.Predicate.NamedFunction
import com.phasmidsoftware.misc.{JPredicate, Predicate}

import scala.language.postfixOps
import scala.util.*

object Checker {
  val flog = Flog[Checker].disabled

  import flog.*

  /**
    * JPredicate to test for a penalty in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Checker
    */
  def penaltyChecker(dir: Boolean): Checker = {
    // CONSIDER rewriting this with just one jLens (using a function to negate AND project. {
    new JPredicate[SB] {
      def justification(sb: SB): Option[String] = s"penaltyChecker($dir, $sb)" !! penaltyStringOpt(sb)
    }.jLens[SB]("")(NamedFunction("negate", sb => sb.negate)).jLens[ScoreVul](if (dir) "NS" else "EW")(NamedFunction(s"project($dir)", sv => sv.project(dir))).andThen(positive(!dir))
  }

  /**
    * JPredicate to test for a game in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Checker
    */
  def gameChecker(dir: Boolean, doubled: Boolean = false): Checker =
    positive(dir) andThen gamePredicate(doubled).jLens[ScoreVul](if (dir) "NS" else "EW")(NamedFunction(s"project ($dir)", u => u.project(dir)))

  /**
    * JPredicate to test for a partial in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Checker
    */
  private def partialChecker(dir: Boolean, doubled: Boolean): Checker = {
    val bonus = if (doubled) 100 else 50
    val value: String = s"""${if (dir) "NS" else "EW"}"""
    positive(dir) andThen trickScorePredicate(doubled).jLensOpt[SB]("partial")(NamedFunction(s"deduct ($bonus)", sb => sb.deduct(bonus))).jLens[ScoreVul](value)(NamedFunction(s"project ($dir)", _.project(dir)))
  }

  private def positive(dir: Boolean): Checker = JPredicate.when[ScoreVul]("")(_.score > 0 == dir)

  private def atLeast(min: Int): Checker = JPredicate.when[ScoreVul](s"score >= $min")(_.score >= min)

  private def atMost(max: Int): Checker = JPredicate.when[ScoreVul](s"score <= $max")(_.score <= max)

  private lazy val Passout: Checker = JPredicate.when("pass out")(z => z.score == 0)
  // CONSIDER we shouldn't have to guess at the direction
  lazy val Partial: Checker = (for (x <- Seq(true, false); y <- Seq(false, true)) yield partialChecker(x, y)) reduce ((a, b) => a orElse b)
  lazy val Penalty: Checker = penaltyChecker(true) orElse penaltyChecker(false)
  lazy val Game: Checker = gameChecker(dir = true) orElse gameChecker(dir = false)
  // CONSIDER we shouldn't have to guess at the direction
  private lazy val DoubledGame: Checker = gameChecker(dir = true, doubled = true) orElse gameChecker(dir = false, doubled = true)

  // TODO do this with reduce of foldLeft
  private def DoubledGameWithOvertricks(dir: Boolean): Checker = overtrickChecker(DoubledGame)(dir, 1) orElse overtrickChecker(DoubledGame)(dir, 2) orElse overtrickChecker(DoubledGame)(dir, 3)

  private def overtrickChecker(checker: Checker)(dir: Boolean, n: Int): Checker = checker.jLensOpt[ScoreVul](s"+$n")(NamedFunction(s"deductForOvertricks($dir,$n)", sv => sv.deductForOvertricks(dir)(n)))

  // NOTE: we accept doubled contracts as OK,
  // NOTE: we accept doubled contracts as OK,
  // but anything redoubled will need to be questioned.
  lazy val Valid: Checker = Passout orElse Game orElse Penalty orElse Partial orElse DoubledGame orElse DoubledGameWithOvertricks(true) orElse DoubledGameWithOvertricks(false)

  // Import Compound for :| method

  import Predicate.Compound

  private def suitPartial(strain: String, value: Int, min: Int, max: Int): JPredicate[Int] =
    (score: Int) => {
      val n = score / value
      s"suitPartial($strain,$value,$max)($score)" !! Option.when(score :| value && Range.inclusive(min, max).contains(n))(s"$n $strain")
    }

  def stripBonus(bonusV: Int, bonusN: Int, min: Int = 0)(sv: SB): Option[Int] = {
    val x = sv.score - (if (sv.vulnerability) bonusV else bonusN)
    Option.when(x >= min)(x)
  }

  private lazy val minorPartial = suitPartial("minor", 20, 1, 7)
  private lazy val majorPartial = suitPartial("major", 30, 1, 7)

  private def notrumpPartial(doubled: Boolean) = new JPredicate[Int]() {
    def justification(score: Int): Option[String] = s"notrumpPartial($doubled, $score )" !!
      (
      if (doubled)
        if (score == 80)
          Some("1 NT")
        else
          suitPartial("NT", 60, 1, 1).justification(score - 20)
      else if (score == 40) Some("1 NT") else suitPartial("NT", 30, 1, 7).justification(score - 10)
        )
  }

  private lazy val doubledMinorPartial = suitPartial("Xminor", 40, 1, 3)
  private lazy val doubledMajorPartial = suitPartial("Xmajor", 60, 1, 4)

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
      (
        if (doubled)
          doubledMajorPartial orElse doubledMinorPartial
        else
          majorPartial orElse minorPartial
        )

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
      if (doubled)
        trickScorePredicate(true).jLensOpt("gameX")(NamedFunction(s"stripBonus doubled (550,350,100)", stripBonus(550, 350, 100))) orElse
          trickScorePredicate(true).jLensOpt("slam")(NamedFunction(s"stripBonus doubled (1300,850,100)", stripBonus(1300, 850, 100))) orElse
          trickScorePredicate(true).jLensOpt("grand slam")(NamedFunction(s"stripBonus doubled (2050,1350,100)", stripBonus(2050, 1350, 100)))
      else
        trickScorePredicate().jLensOpt("game")(NamedFunction(s"stripBonus (500,300)", stripBonus(500, 300, 100))) orElse
          trickScorePredicate().jLensOpt("slam")(NamedFunction(s"stripBonus (1250,800)", stripBonus(1250, 800, 100))) orElse
          trickScorePredicate().jLensOpt("grand slam")(NamedFunction(s"stripBonus (2000,1300)", stripBonus(2000, 1300, 100)))

  /**
    * Determines whether the penalty conditions for a given score and vulnerability (`SB`) meet specific thresholds,
    * and delegates to the `penalty` method for further evaluation.
    * The method examines the score value
    * and checks different scenarios (e.g., match vulnerability and doubling) to compute an optional string (or
    * return `None` if no conditions are met).
    *
    * @param sb An instance of `SB` representing the score value and its associated vulnerability status.
    * @return An optional string providing a description of the penalty if applicable, or `None` if no penalty is deemed valid.
    */
  private def penaltyStringOpt(sb: SB): Option[String] =
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

  /**
    * Computes a descriptive string indicating the number of undertricks and
    * whether the contract was doubled, based on the given vulnerability,
    * doubling state, and score.
    *
    * @param vulnerable a Boolean indicating whether the match is vulnerable.
    * @param doubled    a Boolean specifying whether the contract was doubled.
    * @param score      an integer representing the penalty score that we are trying to match.
    * @return a string indicating the penalties for the specified conditions.
    */
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

  /**
    * Creates a PlayResult instance representing an error or invalid scenario,
    * with the provided error message encapsulated as a Left value.
    *
    * @param s The error message to be encapsulated in the PlayResult.
    * @return A PlayResult instance containing the provided error message.
    */
  def error(s: String): PlayResult = PlayResult(Left(s))
}

/**
  * Represents vulnerability in a game, modeled with two boolean flags.
  *
  * @param ns Indicates North-South vulnerability.
  * @param ew Indicates East-West vulnerability.
  */
case class Vulnerability(ns: Boolean, ew: Boolean) {
  /**
    * Produces a new `Vulnerability` instance with inverted values for the `ns` and `ew` flags.
    *
    * @return A `Vulnerability` instance with `ns` and `ew` flags set to their logical negations.
    */
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
    * Get the vulnerability for a particular board.
    *
    * See https://tedmuller.us/Bridge/Esoterica/BoardVulnerability.htm
    *
    * @param x the board number (1...)
    * @return an instance of `Vulnerability`.
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
  val flog = Flog[ScoreVul].disabled

  import flog.*

  private def vulnerable(direction: Boolean): Boolean = if (direction) vulnerability.ns else vulnerability.ew

  /**
    * Projects the score and determines the vulnerability based on the specified direction.
    *
    * @param direction A boolean value indicating the direction:
    *                  - true for 'ns' direction
    *                  - false for 'ew' direction
    * @return An instance of `SB` containing the score (positive or negative based on the direction)
    *         and the computed vulnerability for the given direction.
    */
  def project(direction: Boolean): SB = SB(if (direction) score else -score, vulnerable(direction))

  /**
    * Deducts points from the score for overtricks based on the player's vulnerability status.
    *
    * @param direction Indicates the direction (true for ns, false for ew) to determine vulnerability.
    * @param n         The number of overtricks to be accounted for in the score deduction.
    * @return An updated `Option[ScoreVul]` with the adjusted score if the deduction is valid,
    *         or `None` if the deduction would result in a negative score.
    */
  def deductForOvertricks(direction: Boolean)(n: Int): Option[ScoreVul] = {
    val x = (if (vulnerable(direction)) 200 else 100) * n

    given Loggable[ScoreVul] = s => s.toString
    s"deductForOvertricks($direction)($n) i=$x" !! Option.when(x < score)(copy(score = score - x))
  }
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
  /**
    * Method to negate the score.
    *
    * @return a new instance of SB with the same vulnerability but the negative score.
    */
  def negate: SB = copy(score = -score)

  /**
    * Method to deduct the given `bonus` from the score and, if positive, return it wrapped in `Some`.
    *
    * @param bonus the value of the bonus.
    * @return `Some(positive value)` or `None`.
    */
  def deduct(bonus: Int): Option[Int] = Option.when(score > bonus)(score - bonus)
}

