/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.PlayResult.{partialOK, scoreOK}
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
    case Right(score) => scoreOK(ScoreVul(score, vul))
  }

  //  object scorePredicate extends ScoreVulPredicate("score", )

  //  case class Predicate(name: String, predicate: Int => Boolean) extends (Int => Boolean)
  //  case class Condition(name: String, predicate: Int => Boolean, conclusion: Int => Boolean) extends (Int => Boolean) {
  //    def apply(x: Int): Boolean = {
  //      val result = predicate(x) && conclusion(x); if (result) println(s"matched $name"); result
  //    }
  //
  //    def orElse(condition: Condition): Condition = Condition(s"$name orElse ${condition.name}", !apply(_), condition)
  //  }

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

  import Predicate._

  private def suitPartial(suit: String, value: Int) = IntPredicate(s"$suit partial", score => score :| value && Range(1, 8).contains(score / value))

  private val minorPartial = suitPartial("minor", 20)
  private val majorPartial = suitPartial("major", 30)
  private val notrumpPartial = IntPredicate("notrump", score => majorPartial(score - 10))
  //  val doubledMinorPartial: IntPredicate = suitPartial("Xminor", 40)
  //  val doubledMajorPartial: IntPredicate = suitPartial("Xmajor", 60)
  //  val doubledNotrumpPartial: IntPredicate = IntPredicate("Xnotrump", score => majorPartial(score - 20))

  private val trickScorePredicate = minorPartial orElse majorPartial orElse notrumpPartial
  //  val partScorePredicate: Predicate[Int] = trickScorePredicate.lens[Int](_ - 50) orElse trickScorePredicate.lens(_ - 100)

  //  val penaltyIsOKPred = SBPredicate("penaltyIsOK", (s,v) => penaltyIsOk(s, v))

  //     NOTE we don't accept doubled overtricks or redoubled contracts here--they must be questioned.
  private val gameP: Predicate[SB] = trickScorePredicate.lens[SB](stripBonus(500, 300)) orElse
    trickScorePredicate.lens[SB](stripBonus(1250, 800)) orElse
    trickScorePredicate.lens[SB](stripBonus(2000, 1400))

  def Game(b: Boolean): Predicate[ScoreVul] = gameP.lens(u => u.project(b))

  val scoreOK: Predicate[ScoreVul] = penaltyP(true) orElse penaltyP(false) orElse Game(true) orElse Game(false) orElse Partial

  private val scoreIsPositive: Predicate[Int] = IntPredicate("positive", _ > 0)

  //  def vulnerabilityPredicate(ns: Boolean): Predicate[Vulnerability] = (v: Vulnerability) => if (ns) v.ns else v.ew

  private val SVIsPositive: Predicate[ScoreVul] = scoreIsPositive.lens(sv => sv.score)
  //  val SVIsPositive2: Predicate[SB] = scoreIsPositive.lens(sb => sb.score)

  private def penaltyP(dir: Boolean): Predicate[ScoreVul] = new BasePredicate[SB]("penalty", penaltyIsOk).lens(sv => sv.project(dir))
  //  val gameIsOKp: Predicate[ScoreVul] =
  //    trickScorePredicate(sv.score)
  //    gameIsOK(sv.score, sv.vulnerability.ns)

  //  val gameOKp: Predicate[ScoreVul] = (SVIsPositive andThen gameP.lens[ScoreVul](sv => sv.project(true))) orElse gameP.lens[ScoreVul](sv => sv.project(true)).flip
  //  val gameOKp: Predicate[SB] = (SVIsPositive2 andThen gameP orElse gameP.flip.lens[ScoreVul](sv => sv.rotate))


  //  def gameOK(sv: ScoreVul): Boolean =
  //    if (sv.score > 0) gameIsOK(sv.score, sv.vulnerability.ns)
  //    else gameIsOK(-score, vul.ew)

  //  val penaltyOKPredicate = ScoreVulPredicate("penaltyOK", sv => if (sv.score > 0) penaltyIsOKPred(sv.score,sv.vulnerability.ew) else penaltyIsOKPred(-sv.score,sv.vulnerability.ns) )
  //  val penaltyOKpredicate =
  //  def penaltyOk(score: Int, vul: Vulnerability): Boolean =
  //    if (score > 0) penaltyIsOk(score, vul.ew)
  //    else penaltyIsOk(-score, vul.ns)

  def partialOK(score: Int, vul: Vulnerability): Boolean = trickScorePredicate(math.abs(score))

  private def stripBonus(bonusV: Int, bonusN: Int)(sv: SB): Int = sv.score - (if (sv.vulnerability) bonusV else bonusN)

  //  private def gameIsOK(score: Int, vul: Boolean): Boolean =
  //  {
  //    val gameBonus = if (vul) 500 else 300
  //    val slamBonus = if (vul) 1250 else 800
  //    val grandSlamBonus = if (vul) 2000 else 1400
  ////     NOTE we don't accept doubled overtricks or redoubled contracts here--they must be questioned.
  //    partScoreIsOK (score-gameBonus) || partScoreIsOK(score-slamBonus) || partScoreIsOK(score-grandSlamBonus) || trickScoreDoubledOK(score-gameBonus)
  //  }


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

case class ScoreVulPredicate(name: String, f: ScoreVul => Boolean) extends BasePredicate[ScoreVul](name, f)

//case class SBPredicate(name: String, f: SB => Boolean) extends BasePredicate[SB](name, f)

//object Penalty extends ScoreVulPredicate("penalty", sv => penaltyOk(sv.score, sv.vulnerability))
object Partial extends ScoreVulPredicate("partial", sv => partialOK(sv.score, sv.vulnerability))

case class ScoreVul(score: Int, vulnerability: Vulnerability) {
  def rotate: ScoreVul = ScoreVul(-score, vulnerability.invert)

  def project(direction: Boolean): SB = if (direction) SB(score, vulnerability.ns) else SB(-score, vulnerability.ew)
}

case class SB(score: Int, vulnerability: Boolean) {
  def rotate: SB = SB(-score, !vulnerability)
}
