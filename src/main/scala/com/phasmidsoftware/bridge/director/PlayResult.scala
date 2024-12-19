/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.Checker.Valid
import com.phasmidsoftware.number.core.Rational

import scala.language.postfixOps
import scala.util._
import scala.util.matching.Regex

/**
  * This is a play result, that's to say either a bridge score (+ or - according to what NS scored)
  * OR a code.
  *
  * @param r Either: an integer (multiple of 10), Or: one of the following:
  *          - DNP: did not play
  *          - A+: N/S got Average plus (60%) and E/W Average minus
  *          - A: B sides got Average (50%)
  *          - A-: N/S got Average minus (40%) and E/W Average plus
  */
case class PlayResult(r: Either[String, Int]) {
  /**
    * Implicit class for case-insensitive matching.
    *
    * @param sc a StringContext.
    */
  implicit class CaseInsensitiveRegex(sc: StringContext) {
    def caseInsensitive: Regex = ("(?i)" + sc.parts.mkString).r
  }

  /**
    * Method to determine if this PlayResult was played (a score or an average ruling).
    *
    * @return false if this is a DNP otherwise true.
    */
  lazy val played: Boolean = r match {
    case Left(caseInsensitive"DNP") => false
    case _ => true
  }

  /**
    * Method to get the matchpoints for this PlayResult
    *
    * @param f call-by-name value of the matchpoints where the result is an Int
    * @return an optional Rational
    */
  def matchpoints(f: => Option[Rational]): Option[Rational] = if (played)
    r match {
      case Right(_) => f
      // NOTE that the the A annotations are NOT case-insensitive
      case Left("A-") => Some(Rational(2, 5))
      case Left("A") => Some(Rational(1, 2))
      case Left("A+") => Some(Rational(3, 5))
      case _ => throw ScoreException(s"matchpoints: unrecognized result: $r")
    }
  else None

  def exists: Boolean = (r match {
    case Right(_) => true
    case Left("A-" | "A" | "A+") => true
    case _ => false
  }) || !played

  def getProbableContract(vul: Vulnerability): Option[String] = r match {
    case Left(w) => Some(w)
    case Right(score) => Valid.justification(ScoreVul(score, vul))
  }
  /**
    * Here we check if the result is one of the common possible results.
    *
    * @param vul the vulnerability.
    */
  def checkScore(vul: Vulnerability): Boolean = getProbableContract(vul).isDefined

  override def toString: String = r match {
    case Left(x) => x
    case Right(x) => x.toString
  }

  def invert: PlayResult = if (exists) PlayResult(r.map(x => -x)) else this
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
}
