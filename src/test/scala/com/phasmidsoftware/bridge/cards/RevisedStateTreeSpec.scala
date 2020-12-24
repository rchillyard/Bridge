/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.decisiontree.tree.Goal
import com.phasmidsoftware.decisiontree.tree.Tree.TreeOps
import com.phasmidsoftware.decisiontree.{Expandable, GoalDriven, StateNode}
import com.phasmidsoftware.output.MockWriter
import com.phasmidsoftware.util.{Loggable, Loggables, Output}
import org.scalatest.flatspec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class RevisedStateTreeSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "RevisedStateTree"

  private val deal0 = Deal("test", 0L, adjustForPartnerships = false)
  private val whist00 = Whist(deal0, 0)
  private val emptyTrick = Trick.empty
  private val initialState = State(whist00, emptyTrick, Tricks.zero)

  it should "apply" in {
    implicit val whistGoal: GoalDriven[State] = Whist.goal(0, _directionNS = true, 1)
    implicit val goal: Goal[State] = Goal.goal(t => t.trick.index > 1)
    val target: RevisedStateTree = RevisedStateTree(initialState)
    target.key.deal shouldBe deal0.quit
    target.key.trick shouldBe emptyTrick
  }

  it should "output" in {
    implicit val whistGoal: GoalDriven[State] = Whist.goal(0, _directionNS = true, 1)
    implicit val goal: Goal[State] = whistGoal.goal
    val target = RevisedStateTree(initialState)
    val writer = MockWriter()
    // NOTE: this is probably not correct as we reject the first element but don't return it as part of the output queue.
    target.output(Output(writer)).close()
    writer.spilled shouldBe 4
  }

  it should "targetedBFS" in {
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val initialState = State(whist, emptyTrick, Tricks.zero)
    implicit val goal: Goal[State] = Goal.goal(t => t.trick.plays.lastOption.exists(_.asCard == Card("HK")))
    val target = RevisedStateTree(initialState)
    val result = target.targetedBFS()
    result should matchPattern { case Some(_) => }
    println(result.get)
  }

  //  it should "expand 3" in {
  //    implicit val sg: GoalDriven[State] = new GoalDriven[State] {
  //      def goalAchieved(t: State): Boolean = t.trick.plays.lastOption.exists(_.asCard == Card("HK"))
  //
  //      def goalImpossible(t: State, moves: Int): Boolean = false
  //    }
  //    implicit val expandable: PlainEnumerationExpandable = new PlainEnumerationExpandable() {}
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(3)
  //    val traverse = result.depthFirstTraverse
  //    traverse.size shouldBe 2
  //    val writer = MockWriter(8192)
  //    result.output(Output(writer)).close()
  //    writer.spillway shouldBe "T0  (7.0) \n  T1 N:HK (6.5)"
  //    writer.spilled shouldBe 26
  //  }
  //
  //  it should "expand 4" in {
  //    implicit val sg: GoalDriven[State] = new GoalDriven[State] {
  //      def goalAchieved(t: State): Boolean = t.tricks.ns >= 1
  //
  //      def goalImpossible(t: State, moves: Int): Boolean = false
  //    }
  //    implicit val expandable: PlainEnumerationExpandable = new PlainEnumerationExpandable() {}
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(4)
  //    result.children.size shouldBe 1
  //    val writer = MockWriter(8192)
  //    result.output(Output(writer)).close()
  //    //		writer.spillway shouldBe "T0  (7.0) \n  T1 N:HK (6.5)"
  //    writer.spilled shouldBe 111
  //    result.depthFirstTraverse.size shouldBe 6
  //  }
  //
  //  it should "expand 5" in {
  //    implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 1)
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(5)
  //    result.children.size shouldBe 1
  //    val writer = MockWriter(16384)
  //    result.output(Output(writer)).close()
  //    writer.spilled shouldBe 111
  //    result.depthFirstTraverse.size shouldBe 6
  //  }
  //
  //  it should "expand 6" in {
  //    implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 2)
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(6)
  //    result.children.size shouldBe 1
  //    val writer = MockWriter(16384)
  //    result.output(Output(writer)).close()
  //    writer.spilled shouldBe 111
  //    result.depthFirstTraverse.size shouldBe 6
  //  }
  //
  //  it should "expand 7" in {
  //    implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 2)
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(7)
  //    result.children.size shouldBe 1
  //    result.depthFirstTraverse.size shouldBe 6
  //  }
  //
  //  it should "expand 9a" in {
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //
  //    implicit val se: Expandable[State] = new OldStyleExpandable(
  //      s =>
  //        s.tricks.ns >= 2,
  //      s =>
  //        s.tricks.ew >= 1)
  //    implicit val whistGoal: GoalDriven[State] = Whist.goal(2, _directionNS = true, 3)
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(9)
  //    val states: Seq[State] = result.depthFirstTraverse
  //    states.size shouldBe 10
  //  }
  //
  //  it should "expand 9b" in {
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //
  //    implicit val se: Expandable[State] = (t: State) => t.enumeratePlays
  //    implicit val sg: GoalDriven[State] = Whist.goal(2, _directionNS = true, 3)
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(9)
  //    val states: Seq[State] = result.depthFirstTraverse
  //    states.size shouldBe 10
  //  }
  //
  //  it should "expand 13" in {
  //    val deal = Deal("test", 2L, adjustForPartnerships = false)
  //    val whist = Whist(deal, 0)
  //
  //    implicit val whistGoal: GoalDriven[State] = Whist.goal(3, _directionNS = true, 4)
  //    implicit val se: Expandable[State] = (t: State) => t.enumeratePlays
  //    val target = StateTree(whist)
  //
  //    val result = target.expand(13)
  //    val states: Seq[State] = result.depthFirstTraverse
  //    states.size shouldBe 13
  //  }

}
