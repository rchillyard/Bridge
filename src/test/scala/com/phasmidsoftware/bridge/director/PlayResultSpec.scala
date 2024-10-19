/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.Vulnerability._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PlayResultSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Game"

  it should "work for -50, N" in {
    val predicate = PlayResult.Game(true)
    val sv: ScoreVul = ScoreVul(-50, Vulnerability.N)
    predicate(sv) shouldBe false
  }

  behavior of "Partial"

  it should "work for -50, N" in {
    val sv: ScoreVul = ScoreVul(-50, Vulnerability.N)
    Partial(sv) shouldBe false
  }

  behavior of "PlayResult"
  it should "checkScore partial" in {
    PlayResult(80).checkScore(B) shouldBe true
    PlayResult(-80).checkScore(B) shouldBe true
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
    //    PlayResult(-50).checkScore(O) shouldBe true
    PlayResult(-50).checkScore(N) shouldBe true
    PlayResult(100).checkScore(E) shouldBe true
    PlayResult(-200).checkScore(N) shouldBe true
    PlayResult(-1100).checkScore(B) shouldBe true
  }
}
