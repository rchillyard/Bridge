/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.Vulnerability._
import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.output.MockWriter
import com.phasmidsoftware.util.Output
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PlayResultSpec extends AnyFlatSpec with should.Matchers {

  behavior of "PlayResult"
  it should """apply("110")""" in {
    val target = PlayResult("110")
    target.r.isRight shouldBe true
    target.r.toOption should matchPattern { case Some(110) => }
    target.toString shouldBe "110"
  }
  it should """apply(Right(110))""" in {
    val target = PlayResult(Right(110))
    target.r.isRight shouldBe true
    target.r.toOption should matchPattern { case Some(110) => }
    target.toString shouldBe "110"
  }
  it should "apply(Left(DNP))" in {
    val target = PlayResult(Left("DNP"))
    target.r.isRight shouldBe false
    target.r.toOption should matchPattern { case None => }
    target.matchpoints(None) shouldBe None
    target.toString shouldBe "DNP"
  }
  it should "apply(DNP)" in {
    val target = PlayResult("DNP")
    target.r.isRight shouldBe false
    target.r.toOption should matchPattern { case None => }
    target.matchpoints(None) shouldBe None
    target.toString shouldBe "DNP"
  }
  it should "apply(A)" in {
    val target = PlayResult("A")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational.half)
    target.toString shouldBe "A"
  }
  it should "apply(A+)" in {
    val target = PlayResult("A+")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational(3) / 5)
    target.toString shouldBe "A+"
  }
  it should "apply(A-)" in {
    val target = PlayResult("A-")
    target.r.isRight shouldBe false
    target.matchpoints(None) shouldBe Some(Rational(2) / 5)
    target.toString shouldBe "A-"
  }
  it should "get probable contract 1" in {
    val target = PlayResult(110)
    target.getProbableContract(Vulnerability(1)) shouldBe Some("NS partial 2 major")
  }
  it should "output" in {
    val score1 = "110"
    val score2 = "100"
    val result1 = PlayResult(score1)
    val result2 = PlayResult(score2)
    val bd = 5
    val ns1 = 13
    val ns2 = 14
    val ew1 = 17
    val ew2 = 16
    val play1 = Play(ns1, ew1, result1)
    val play2 = Play(ns2, ew2, result2)
    val target = Traveler(bd, List(play1, play2), None)
    target.board shouldBe bd
    target.ps.size shouldBe 2
    target.ps shouldBe List(play1, play2)
    target.isPlayed shouldBe true
    target.top shouldBe 1
    val writer = MockWriter()
    target.output(Output(writer)).close()
    writer.spillway shouldBe
      s"""Board: 5 with 2 plays
         |NS pair	EW pair	NS score	NS MPs
         |13	17	110	 1.00 (probable contract: NS partial 2 major)
         |14	16	100	 0.00 (probable contract: EW  down 2)
         |""".stripMargin
  }

  behavior of "checkScore"

  it should "checkScore partial" in {
    PlayResult(80).checkScore(B) shouldBe true
    PlayResult(-80).checkScore(B) shouldBe true
  }
  it should "checkScore doubled partial" in {
    PlayResult(160).checkScore(B) shouldBe true
    PlayResult(-160).checkScore(B) shouldBe true
    PlayResult(160).checkScore(B) shouldBe true
    PlayResult(-160).checkScore(B) shouldBe true
  }
  it should "checkScore game" in {
    PlayResult(420).checkScore(O) shouldBe true
    PlayResult(450).checkScore(E) shouldBe true
    PlayResult(-420).checkScore(N) shouldBe true
    PlayResult(-680).checkScore(B) shouldBe true
  }
  it should "checkScore slam" in {
    PlayResult(980).checkScore(O) shouldBe true
    PlayResult(1010).checkScore(E) shouldBe true
    PlayResult(-920).checkScore(N) shouldBe true
    PlayResult(-1370).checkScore(B) shouldBe true
  }
  it should "checkScore penalty" in {
    PlayResult(-50).checkScore(O) shouldBe true
    PlayResult(100).checkScore(E) shouldBe true
    PlayResult(-200).checkScore(N) shouldBe true
    PlayResult(-1100).checkScore(B) shouldBe true
  }
  it should "fail some scores" in {
    PlayResult(10).checkScore(B) shouldBe false
    PlayResult(-10).checkScore(B) shouldBe false
    PlayResult(20).checkScore(B) shouldBe false
    PlayResult(-20).checkScore(B) shouldBe false
    PlayResult(30).checkScore(B) shouldBe false
    PlayResult(-30).checkScore(B) shouldBe false
    PlayResult(40).checkScore(B) shouldBe false
    PlayResult(-40).checkScore(B) shouldBe false
    PlayResult(60).checkScore(B) shouldBe false
    PlayResult(-60).checkScore(B) shouldBe false
  }

}
