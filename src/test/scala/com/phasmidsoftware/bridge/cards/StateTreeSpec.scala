/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.decisiontree.{Expandable, GoalDriven, StateNode}
import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.output.MockWriter
import com.phasmidsoftware.util.{Loggables, Output}
import org.scalatest.flatspec
import org.scalatest.matchers.should

//noinspection ScalaStyle
class StateTreeSpec extends flatspec.AnyFlatSpec with should.Matchers {

  class OldStyleExpandable(success: State => Boolean = _ => false, failure: State => Boolean = _ => false) extends Expandable[State] with Loggables {

    implicit val optionLoggerBoolean: Loggable[Option[Boolean]] = optionLoggable[Boolean]
    implicit val ListLoggerState: Loggable[List[State]] = listLoggable[State]

    import com.phasmidsoftware.util.SmartValueOps._

    def successors(t: State): List[State] = t.enumeratePlays.invariant(xs => xs.distinct.length == xs.length) to List
  }

  class PlainEnumerationExpandable() extends Expandable[State] with Loggables {
    def successors(t: State): List[State] = t.enumeratePlays to List
  }

  behavior of "Tree"

  private val deal0 = Deal("test", 0L, adjustForPartnerships = false)
  private val whist00 = Whist(deal0, 0)

  def alwaysNone(n: State): Option[Boolean] = None

  implicit val se: Expandable[State] = new OldStyleExpandable()

  it should "apply" in {
    // TODO sort this out properly.
    val trick = Trick.empty
    implicit val whistGoal: GoalDriven[State] = Whist.goal(0, _directionNS = true, 1)
    val root = StateNode(State(whist00, trick, Tricks.zero), so = None, Nil)
    val target = StateTree(root)
    target.root.state.deal shouldBe deal0.quit
    target.root.state.trick shouldBe trick
    target.root shouldBe root
  }

  it should "output" in {
    implicit val whistGoal: GoalDriven[State] = Whist.goal(0, _directionNS = true, 1)
    val target = StateTree(whist00)
    val writer = MockWriter()
    target.output(Output(writer)).close()
    writer.spilled shouldBe 9
  }

  // TODO sort this out.
//  it should "enumerateFollows" in {
//    val deal = Deal("test", 2L)
//    deal.output(Output(System.out)).close()
//    val whist = Whist(deal, 0)
//    val trick = Trick(1, List(CardPlay(deal, 0, Spades, 1)), None)
//    val state = State(whist, trick)
//    val ss = state.enumerateFollows
//    ss.size shouldBe 2
//    ss.head.trick.toString shouldBe "T1 {SK, S4}"
//    ss.last.trick.toString shouldBe "T1 {SK, SJ}"
//  }

  // TODO restore me
  ignore should "expand 1" in {
    implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 1)
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val target = StateTree(whist)

    val result = target.expand(1)
    result.children.size shouldBe 0
    val writer = MockWriter()
    result.output(Output(writer)).close()
    writer.spilled shouldBe 9
    writer.spillway shouldBe "T0  (7.0)"
    val traverse = result.depthFirstTraverse
    traverse.size shouldBe 1
  }

  it should "expand 2" in {
    implicit val sg: GoalDriven[State] = new GoalDriven[State] {
      def goalAchieved(t: State): Boolean = t.trick.plays.lastOption.exists(_.asCard == Card("HK"))

      def goalImpossible(t: State, moves: Int): Boolean = false
    }
    implicit val expandable: PlainEnumerationExpandable = new PlainEnumerationExpandable() {}
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val target = StateTree(whist)

    val result = target.expand(2)
    result.children.size shouldBe 1
    val states = result.depthFirstTraverse
    states.size shouldBe 2
    val writer = MockWriter()
    result.output(Output(writer)).close()
    writer.spilled shouldBe 26
    writer.spillway shouldBe "T0  (7.0) \n  T1 N:HK (6.5)"
  }

  it should "expand 3" in {
    implicit val sg: GoalDriven[State] = new GoalDriven[State] {
      def goalAchieved(t: State): Boolean = t.trick.plays.lastOption.exists(_.asCard == Card("HK"))

      def goalImpossible(t: State, moves: Int): Boolean = false
    }
    implicit val expandable: PlainEnumerationExpandable = new PlainEnumerationExpandable() {}
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val target = StateTree(whist)

    val result = target.expand(3)
    val traverse = result.depthFirstTraverse
    traverse.size shouldBe 2
    val writer = MockWriter(8192)
    result.output(Output(writer)).close()
    writer.spillway shouldBe "T0  (7.0) \n  T1 N:HK (6.5)"
    writer.spilled shouldBe 26
  }

  it should "expand 4" in {
    implicit val sg: GoalDriven[State] = new GoalDriven[State] {
      def goalAchieved(t: State): Boolean = t.tricks.ns >= 1

      def goalImpossible(t: State, moves: Int): Boolean = false
    }
    implicit val expandable: PlainEnumerationExpandable = new PlainEnumerationExpandable() {}
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val target = StateTree(whist)

    val result = target.expand(4)
    result.children.size shouldBe 1
    val writer = MockWriter(8192)
    result.output(Output(writer)).close()
    //		writer.spillway shouldBe "T0  (7.0) \n  T1 N:HK (6.5)"
    writer.spilled shouldBe 111
    result.depthFirstTraverse.size shouldBe 6
  }

  it should "expand 5" in {
    implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 1)
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val target = StateTree(whist)

    val result = target.expand(5)
    result.children.size shouldBe 1
    val writer = MockWriter(16384)
    result.output(Output(writer)).close()
    writer.spilled shouldBe 111
    result.depthFirstTraverse.size shouldBe 6
  }

  it should "expand 6" in {
    implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 2)
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val target = StateTree(whist)

    val result = target.expand(6)
    result.children.size shouldBe 1
    val writer = MockWriter(16384)
    result.output(Output(writer)).close()
    writer.spilled shouldBe 111
    result.depthFirstTraverse.size shouldBe 6
  }

  it should "expand 7" in {
    implicit val whistGoal: GoalDriven[State] = Whist.goal(1, _directionNS = true, 2)
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)
    val target = StateTree(whist)

    val result = target.expand(7)
    result.children.size shouldBe 1
    result.depthFirstTraverse.size shouldBe 6
  }

  it should "expand 9a" in {
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)

    implicit val se: Expandable[State] = new OldStyleExpandable(
      s =>
        s.tricks.ns >= 2,
      s =>
        s.tricks.ew >= 1)
    implicit val whistGoal: GoalDriven[State] = Whist.goal(2, _directionNS = true, 3)
    val target = StateTree(whist)

    val result = target.expand(9)
    val states: List[State] = result.depthFirstTraverse
    states.size shouldBe 10
  }

  it should "expand 9b" in {
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)

    implicit val se: Expandable[State] = (t: State) => t.enumeratePlays to List
    implicit val sg: GoalDriven[State] = Whist.goal(2, _directionNS = true, 3)
    val target = StateTree(whist)

    val result = target.expand(9)
    val states: List[State] = result.depthFirstTraverse
    states.size shouldBe 10
  }

  it should "expand 13" in {
    val deal = Deal("test", 2L, adjustForPartnerships = false)
    val whist = Whist(deal, 0)

    implicit val whistGoal: GoalDriven[State] = Whist.goal(3, _directionNS = true, 4)
    implicit val se: Expandable[State] = (t: State) => t.enumeratePlays to List
    val target = StateTree(whist)

    val result = target.expand(13)
    val states: List[State] = result.depthFirstTraverse
    states.size shouldBe 13
  }

}
