/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.PlayResult.Valid
import com.phasmidsoftware.misc.{BasePredicate, IntPredicate, Predicate}
import com.phasmidsoftware.number.core.Rational

import scala.language.postfixOps
import scala.util._

/**
  * This is a play result, that's to say either a bridge score (+ or - according to what NS scored)
  * OR a code.
  *
  * @param r Either: an integer (multiple of 10), Or: one of the following:
  *          DNP: did not play
  *          A+: N/S got Average plus (60%) and E/W Average minus
  *          A: B sides got Average (50%)
  *          A-: N/S got Average minus (40%) and E/W Average plus
  *
  */
case class PlayResult(r: Either[String, Int]) {
  /**
    * Method to get the matchpoints for this PlayResult
    *
    * @param f call-by-name value of the matchpoints where the result is an Int
    * @return an optional Rational
    */
  def matchpoints(f: => Option[Rational]): Option[Rational] = r match {
    case Right(_) => f
    case Left("A-") => Some(Rational(2, 5))
    case Left("A") => Some(Rational(1, 2))
    case Left("A+") => Some(Rational(3, 5))
    case Left("DNP") => None
    case _ => throw ScoreException(s"matchpoints: unrecognized result: $r")
  }

  def exists: Boolean = r match {
    case Right(_) => true
    case Left("A-" | "A" | "A+" | "DNP") => true
    case _ => false
  }

  /**
    * Here we check if the result is one of the common possible results.
    *
    * @param vul the vulnerability.
    */
  def checkScore(vul: Vulnerability): Boolean = r match {
    case Left(_) => true
    case Right(score) => Valid(ScoreVul(score, vul))
  }

  override def toString: String = r match {
    case Left(x) => x
    case Right(x) => x.toString
  }
}

/**
  * Companion object to PlayResult class.
  */
object PlayResult {
  def apply(x: Int): PlayResult = PlayResult(Right(x))

  def apply(s: String): PlayResult = {
    Try(s.toInt).toEither match {
      case Left(_) => PlayResult(Left(s)) // we ignore the exception because it is probably just a non-integer
      case Right(r) => PlayResult(r)
    }
  }

  /**
    * Predicate to test for a penalty in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Predicate[ScoreVul]
    */
  def penaltyP(dir: Boolean): Predicate[ScoreVul] = PenaltyPredicate("penalty", penaltyIsOk).lens[SB](sb => sb.negate).lens(sv => sv.project(dir))

  /**
    * Predicate to test for a game in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Predicate[ScoreVul]
    */
  def gameP(dir: Boolean): Predicate[ScoreVul] = gameP.lens(u => u.project(dir))

  /**
    * Predicate to test for a partial in the direction dir.
    *
    * @param dir true if the direction asked about is NS.
    * @return a Predicate[ScoreVul]
    */
  def partialP(dir: Boolean, doubled: Boolean): Predicate[ScoreVul] = {
    val bonus = if (doubled) 100 else 50
    trickScorePredicate.lens[SB](stripBonus(bonus, bonus)).lens(_.project(dir))
  }

  lazy val Partial: Predicate[ScoreVul] = (for (x <- Seq(true, false); y <- Seq(true, false)) yield partialP(x, y)) reduce ((a, b) => a orElse b)
  lazy val Penalty: Predicate[ScoreVul] = penaltyP(true) orElse penaltyP(false)
  lazy val Game: Predicate[ScoreVul] = gameP(true) orElse gameP(false)
  lazy val Valid: Predicate[ScoreVul] = Penalty orElse Game orElse Partial

  import Predicate._

  private def suitPartial(suit: String, value: Int) = IntPredicate(s"$suit partial", score => score :| value && Range(1, 8).contains(score / value))

  private def stripBonus(bonusV: Int, bonusN: Int)(sv: SB): Int = sv.score - (if (sv.vulnerability) bonusV else bonusN)

  private lazy val minorPartial = suitPartial("minor", 20)
  private lazy val majorPartial = suitPartial("major", 30)
  private lazy val notrumpPartial = majorPartial.lens[Int](_ - 10)
  private lazy val doubledMinorPartial = suitPartial("Xminor", 40)
  private lazy val doubledMajorPartial = suitPartial("Xmajor", 60)
  private lazy val doubledNotrumpPartial = doubledMajorPartial.lens[Int](_ - 20)

  // NOTE we don't accept doubled overtricks or redoubled contracts here--they must be questioned.
  private lazy val trickScorePredicate = minorPartial orElse majorPartial orElse notrumpPartial orElse doubledMinorPartial orElse doubledMajorPartial orElse doubledNotrumpPartial

  private lazy val gameP: Predicate[SB] = trickScorePredicate.lens[SB](stripBonus(500, 300)) orElse
    trickScorePredicate.lens[SB](stripBonus(1250, 800)) orElse
    trickScorePredicate.lens[SB](stripBonus(2000, 1300))

  // NOTE: we accept doubled contracts as OK, but anything redoubled needs to be questioned.
  private def penaltyIsOk(sv: SB): Boolean =
    sv.score match {
      case 50 | 150 | 250 | 350 | 450 | 550 | 650 => !sv.vulnerability
      case 700 | 900 | 1000 | 1200 | 1300 => sv.vulnerability
      case 100 | 200 | 300 | 400 | 500 | 600 | 800 | 1100 | 1400 | 1700 | 2000 | 2300 | 2600 | 2900 | 3200 | 3500 => true
      case 3800 => sv.vulnerability
      case _ => false
    }

  def error(s: String): PlayResult = PlayResult(Left(s))
}

case class Vulnerability(ns: Boolean, ew: Boolean) {
  def invert: Vulnerability = Vulnerability(!ns, !ew)
}

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

case class PenaltyPredicate(name: String, f: SB => Boolean) extends BasePredicate[SB](name, f)

case class ScoreVul(score: Int, vulnerability: Vulnerability) {
  def project(direction: Boolean): SB = if (direction) SB(score, vulnerability.ns) else SB(-score, vulnerability.ew)
}

case class SB(score: Int, vulnerability: Boolean) {
  def negate: SB = copy(score = -score)
}
