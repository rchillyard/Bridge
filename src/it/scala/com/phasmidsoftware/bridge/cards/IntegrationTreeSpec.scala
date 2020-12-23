package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.decisiontree.{Expandable, GoalDriven, StateNode}

//noinspection ScalaStyle
class IntegrationTreeSpec extends flatspec.AnyFlatSpec with should.Matchers {

  def success(n: StateNode[State]): Boolean = false

  behavior of "expand"

  private val deal2 = Deal("test", 2L, adjustForPartnerships = false)
  private val whist = Whist(deal2, 0)

  implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 1)

  class PlainEnumerationExpandable() extends Expandable[State] {
    def successors(t: State): List[State] = t.enumeratePlays
  }

  implicit val se: Expandable[State] = new PlainEnumerationExpandable()

  it should "go to level 12" in {
    StateTree(whist).expand(12).depthFirstTraverse.size shouldBe 6
  }

  // NOTE: the following test actually works but it takes a long time!
  it should "go to level 16" in {
    StateTree(whist).expand(16).depthFirstTraverse.size shouldBe 6
  }

  it should "go to level 16 with short circuit" in {
    val target = StateTree(whist)
    //		val result = target.expand(16)(_.tricks.ns >= 3, _.tricks.ew > 2)
    val result = target.expand(16)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 20 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(20)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 24 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(24)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 28 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(28)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 32 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(32)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 36 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(36)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 40 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(40)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 40 with short alternative circuit" in {
    val target = StateTree(whist)
    val result = target.expand(40)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 44 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(44)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go to level 48 with short circuit" in {
    val target = StateTree(whist)
    val result = target.expand(48)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }

  it should "go through all levels with short circuit based on 3NT" in {
    val target = StateTree(whist)
    val result = target.enumerateNoTrumpPlaysNS(9)
    val states: Seq[State] = result.depthFirstTraverse
    states.size shouldBe 6
  }
}
