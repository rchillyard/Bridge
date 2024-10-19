/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.bridge.director.PlayResult.{Partial, Penalty, penaltyP}
import com.phasmidsoftware.bridge.director.Vulnerability._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PlayResultSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Game"

  it should "work for game NS" in {
    PlayResult.gameP(true)(ScoreVul(600, Vulnerability.N)) shouldBe true
    PlayResult.gameP(true)(ScoreVul(620, Vulnerability.N)) shouldBe true
    PlayResult.gameP(true)(ScoreVul(450, Vulnerability.E)) shouldBe true
    PlayResult.gameP(true)(ScoreVul(480, Vulnerability.E)) shouldBe true
    PlayResult.gameP(true)(ScoreVul(1430, Vulnerability.N)) shouldBe true
    PlayResult.gameP(true)(ScoreVul(2210, Vulnerability.N)) shouldBe true
    PlayResult.gameP(true)(ScoreVul(-50, Vulnerability.N)) shouldBe false
  }
  it should "work for game EW" in {
    PlayResult.gameP(false)(ScoreVul(-600, Vulnerability.E)) shouldBe true
    PlayResult.gameP(false)(ScoreVul(-620, Vulnerability.E)) shouldBe true
    PlayResult.gameP(false)(ScoreVul(-650, Vulnerability.E)) shouldBe true
    PlayResult.gameP(false)(ScoreVul(-680, Vulnerability.E)) shouldBe true
    PlayResult.gameP(false)(ScoreVul(-980, Vulnerability.N)) shouldBe true
    PlayResult.gameP(false)(ScoreVul(-1510, Vulnerability.N)) shouldBe true
    PlayResult.gameP(false)(ScoreVul(50, Vulnerability.E)) shouldBe false
  }

  behavior of "Penalty"

  it should "work for NS penalty when not vul" in {
    Penalty(ScoreVul(50, Vulnerability.O)) shouldBe true
    Penalty(ScoreVul(-50, Vulnerability.O)) shouldBe true
  }

  behavior of "penaltyP"

  it should "work for NS penalty when not vul" in {
    penaltyP(true)(ScoreVul(-50, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-100, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-150, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-200, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-250, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-300, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-350, Vulnerability.O)) shouldBe true
  }

  it should "work for NS penalty when vul" in {
    penaltyP(true)(ScoreVul(-50, Vulnerability.N)) shouldBe false
    penaltyP(true)(ScoreVul(-100, Vulnerability.N)) shouldBe true
    penaltyP(true)(ScoreVul(-150, Vulnerability.N)) shouldBe false
    penaltyP(true)(ScoreVul(-200, Vulnerability.N)) shouldBe true
  }

  it should "work for NS doubled penalty when not vul" in {
    penaltyP(true)(ScoreVul(-100, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-300, Vulnerability.O)) shouldBe true
    penaltyP(true)(ScoreVul(-500, Vulnerability.O)) shouldBe true
  }

  it should "work for NS doubled penalty when vul" in {
    penaltyP(true)(ScoreVul(-200, Vulnerability.N)) shouldBe true
    penaltyP(true)(ScoreVul(-500, Vulnerability.N)) shouldBe true
    penaltyP(true)(ScoreVul(-800, Vulnerability.N)) shouldBe true
    penaltyP(true)(ScoreVul(-1100, Vulnerability.N)) shouldBe true
  }

  behavior of "Partial"

  it should "work for NS notrump partials" in {
    Partial(ScoreVul(90, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(120, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(150, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(180, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(210, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(240, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(270, Vulnerability.N)) shouldBe true
  }

  it should "work for EW notrump partials" in {
    Partial(ScoreVul(-90, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-120, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-150, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-180, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-210, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-240, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-270, Vulnerability.N)) shouldBe true
  }

  it should "work for NS partials" in {
    Partial(ScoreVul(70, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(80, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(110, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(140, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(170, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(200, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(230, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(260, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-50, Vulnerability.N)) shouldBe false
  }

  it should "work for EW partials" in {
    Partial(ScoreVul(-70, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-80, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-110, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-140, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-170, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-200, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-230, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(-260, Vulnerability.N)) shouldBe true
    Partial(ScoreVul(50, Vulnerability.N)) shouldBe false
  }

  behavior of "PlayResult"

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
