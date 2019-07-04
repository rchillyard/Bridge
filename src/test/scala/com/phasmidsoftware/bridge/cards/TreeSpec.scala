/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.{Expandable, GoalDriven}
import com.phasmidsoftware.output.{Loggable, Loggables, MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class TreeSpec extends FlatSpec with Matchers {

	class OldStyleExpandable(success: State => Boolean = _ => false, failure: State => Boolean = _ => false) extends Expandable[State] with Loggables {

		implicit val optionLoggerBoolean: Loggable[Option[Boolean]] = optionLoggable[Boolean]
		implicit val seqLoggerState: Loggable[Seq[State]] = sequenceLoggable[State]

		def successors(t: State): Seq[State] = t.enumeratePlays
	}

	class PlainEnumerationExpandable() extends Expandable[State] with Loggables {
		def successors(t: State): Seq[State] = t.enumeratePlays
	}

	behavior of "Tree"

	private val deal0 = Deal("test", 0L)
	private val whist00 = Whist(deal0, 0)

  def alwaysNone(n: State): Option[Boolean] = None

	implicit val se: Expandable[State] = new OldStyleExpandable()

	it should "apply" in {
		// TODO sort this out properly.
		val trick = Trick.empty
		implicit val irrelevant: GoalDriven[State] = Whist.goal(0, directionNS = true, 1)
		val root = StateNode(State(whist00, trick, Tricks.zero), so = None, Nil)
		val target = Tree(root)
		target.root.state.deal shouldBe deal0
		target.root.state.trick shouldBe trick
		target.root shouldBe root
	}

	it should "chooseLeads" in {
		implicit val irrelevant: GoalDriven[State] = Whist.goal(0, directionNS = true, 1)
		val state = State(whist00)
		val target = Tree(state)
		val result: Seq[CardPlay] = state.chooseLeads(0)
		result.size shouldBe 3
		result.head shouldBe CardPlay(deal0, 0, Hearts, 10)
		result(1) shouldBe CardPlay(deal0, 0, Hearts, 5)
		result.last shouldBe CardPlay(deal0, 0, Hearts, 2)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		writer.spilled shouldBe 9
	}

	it should "output" in {
		implicit val irrelevant: GoalDriven[State] = Whist.goal(0, directionNS = true, 1)
		val target = Tree(whist00)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		writer.spilled shouldBe 9
	}

	it should "enumerateLeads 1" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val state = State(whist)
		// NOTE: Figure out all the possible leads from the North's longest and strongest suit.
		// Bear in mind that we consider all cards from a "sequence" equivalent.
		val ss: Seq[State] = state.enumerateLeads(0, 0)
		ss.size shouldBe 4
		ss.head should matchPattern { case State(_, _, _) => }
		ss.head.trick.toString shouldBe "T0 {Play: 0 H2}"
		ss.tail.head.trick.toString shouldBe "T0 {Play: 0 H5}"
		ss.init.last.trick.toString shouldBe "T0 {Play: 0 H9}"
		ss.last.trick.toString shouldBe "T0 {Play: 0 HK}"
	}

	it should "enumerateLeads 2" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val hands = deal.hands
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick = Trick.create(0, CardPlay(deal, 0, Spades, priority1S), CardPlay(deal, 1, Spades, priority2S), CardPlay(deal, 2, Spades, priority3S), CardPlay(deal, 3, Spades, priority4S))
		// TODO need to sort this out.
		//		val deal2 = deal.playAll(trick)
		trick.winner match {
			case Some(Winner(p, true)) =>
				val state = State(whist, trick, Tricks(0, 0).increment(trick))
				val ss: Seq[State] = state.enumerateLeads(p.hand, trick.index + 1)
				ss.size shouldBe 4
				ss.head should matchPattern { case State(_, _, _) => }
				ss.head.trick.toString shouldBe "T1 {Play: 0 H2}"
				ss.tail.head.trick.toString shouldBe "T1 {Play: 0 H5}"
				ss.init.last.trick.toString shouldBe "T1 {Play: 0 H9}"
				ss.last.trick.toString shouldBe "T1 {Play: 0 HK}"
			case _ => fail("no winner")
		}

	}

	it should "enumerateFollows" in {
		val deal = Deal("test", 2L)
		deal.output(Output(System.out)).close()
		val whist = Whist(deal, 0)
		val trick = Trick(1, Seq(CardPlay(deal, 0, Spades, 1)))
		val state = State(whist, trick)
		val ss = state.enumerateFollows
		ss.size shouldBe 2
		ss.head.trick.toString shouldBe "T1 {Play: 0 SK, Play: 1 SQ}"
		ss.last.trick.toString shouldBe "T1 {Play: 0 SK, Play: 1 S4}"
	}

	it should "expand 1" in {
		implicit val irrelevant: GoalDriven[State] = Whist.goal(1, directionNS = true, 1)
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		val result = target.expand(1)
		result.children.size shouldBe 0
		val writer = MockWriter()
		result.output(Output(writer)).close()
		writer.spilled shouldBe 9
		writer.spillway shouldBe "T0  (7.0)"
		val traverse = result.depthFirstTraverse
		traverse.size shouldBe 1
		//		traverse foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

	it should "expand 2" in {
		implicit val sg: GoalDriven[State] = new GoalDriven[State] {
			def goalAchieved(t: State): Boolean = t.trick.plays.lastOption.exists(_.asCard == Card("HK"))

			def goalImpossible(t: State, moves: Int): Boolean = false
		}
		implicit val expandable: PlainEnumerationExpandable = new PlainEnumerationExpandable() {}
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

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
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

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
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		val result = target.expand(4)
		println(result)
		result.children.size shouldBe 1
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		//		writer.spillway shouldBe "T0  (7.0) \n  T1 N:HK (6.5)"
		writer.spilled shouldBe 111
		result.depthFirstTraverse.size shouldBe 6
	}

	ignore should "expand 5" in {
		implicit val irrelevant: GoalDriven[State] = Whist.goal(2, directionNS = true, 2)
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		val result = target.expand(5)
		result.children.size shouldBe 4
		val writer = MockWriter(16384)
		result.output(Output(writer)).close()
		writer.spilled shouldBe 1086
		result.depthFirstTraverse.size shouldBe 53
	}

	ignore should "expand 6" in {
		implicit val irrelevant: GoalDriven[State] = Whist.goal(2, directionNS = true, 2)
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		val result = target.expand(6)
		result.children.size shouldBe 4
		val writer = MockWriter(16384)
		result.output(Output(writer)).close()
		writer.spilled shouldBe 2838
		result.depthFirstTraverse.size shouldBe 125
	}

	ignore should "expand 7" in {
		implicit val irrelevant: GoalDriven[State] = Whist.goal(2, directionNS = true, 2)
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		val result = target.expand(7)
		result.children.size shouldBe 4
		result.depthFirstTraverse.size shouldBe 341
	}

	it should "expand 9a" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)

		implicit val se: Expandable[State] = new OldStyleExpandable(
			s =>
				s.tricks.ns >= 2,
			s =>
				s.tricks.ew >= 1)
		implicit val irrelevant: GoalDriven[State] = Whist.goal(2, directionNS = true, 3)
		val target = Tree(whist)

		val result = target.expand(9)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 10
	}

	it should "expand 9b" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)

		implicit val se: Expandable[State] = (t: State) => t.enumeratePlays
		implicit val sg: GoalDriven[State] = Whist.goal(2, directionNS = true, 3)
		val target = Tree(whist)

		val result = target.expand(9)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 10
	}

	ignore should "expand 13" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)

		// TODO not irrelevant
		implicit val irrelevant: GoalDriven[State] = Whist.goal(3, directionNS = true, 4)
		implicit val se: Expandable[State] = (t: State) => t.enumeratePlays
		val target = Tree(whist)

		val result = target.expand(13)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 31
		// FIXME
		states foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

}
