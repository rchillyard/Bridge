package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

	behavior of "Tree"

	private val deal0 = Deal("test", 0L)
	private val whist00 = Whist(deal0, 0)

	it should "apply" in {
		// TODO sort this out properly.
		val trick = Trick(0, Nil)
		val root = StateNode(State(whist00, trick, Tricks.zero), done = false, Nil)
		val target = Tree(root)
		target.root.state.deal shouldBe deal0
		target.root.state.trick shouldBe trick
		target.root shouldBe root
	}

	it should "chooseLead" in {
		val state = State(whist00)
		val target = Tree(state)
		val result: Seq[CardPlay] = state.chooseLead(0)
		result.size shouldBe 3
		result.head shouldBe CardPlay(deal0, 0, Hearts, 2)
		result(1) shouldBe CardPlay(deal0, 0, Hearts, 5)
		result.last shouldBe CardPlay(deal0, 0, Hearts, 10)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		writer.spilled shouldBe 9
	}

	it should "output" in {
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
		ss.head.trick.toString shouldBe "T0 {Play: 0 HK}"
		ss.tail.head.trick.toString shouldBe "T0 {Play: 0 H9}"
		ss.init.last.trick.toString shouldBe "T0 {Play: 0 H5}"
		ss.last.trick.toString shouldBe "T0 {Play: 0 H2}"
	}

	it should "enumerateLeads 2" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val hands = deal.hands
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick = Trick.create(0, 0, Spades, CardPlay(deal, 0, Spades, priority1S), CardPlay(deal, 1, Spades, priority2S), CardPlay(deal, 2, Spades, priority3S), CardPlay(deal, 3, Spades, priority4S))
		// TODO need to sort this out.
		val deal2 = deal.playAll(trick)
		trick.winner match {
			case Some(winner) =>
				val state = State(whist, trick, Tricks(0, 0).increment(trick))
				val ss: Seq[State] = state.enumerateLeads(winner, trick.index + 1)
				ss.size shouldBe 4
				ss.head should matchPattern { case State(_, _, _) => }
				ss.head.trick.toString shouldBe "T1 {Play: 0 HK}"
				ss.tail.head.trick.toString shouldBe "T1 {Play: 0 H9}"
				ss.init.last.trick.toString shouldBe "T1 {Play: 0 H5}"
				ss.last.trick.toString shouldBe "T1 {Play: 0 H2}"
			case None => fail("no winner")
		}

	}

	it should "enumerateFollows" in {
		val deal = Deal("test", 2L)
		deal.output(Output(System.out)).close()
		val whist = Whist(deal, 0)
		val trick = Trick(0, Seq(CardPlay(deal, 0, Spades, 1)))
		val state = State(whist, trick)
		val ss = state.enumerateFollows
		ss.size shouldBe 2
		ss.head.trick.toString shouldBe "T0 {Play: 0 SK, Play: 1 SQ}"
		ss.last.trick.toString shouldBe "T0 {Play: 0 SK, Play: 1 S4}"
	}

	it should "enumeratePlays 1" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)
		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(1)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 4
		val writer = MockWriter()
		result.output(Output(writer)).close()
		writer.spilled shouldBe 74
		val traverse = result.depthFirstTraverse
		traverse.size shouldBe 5
		traverse foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

	it should "enumeratePlays 2" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(2)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 4
		val states = result.depthFirstTraverse
		states.size shouldBe 17
		states foreach { s => println(s"${s.trick} ${s.tricks}") }
		val writer = MockWriter()
		result.output(Output(writer)).close()
		writer.spilled shouldBe 294
	}

	it should "enumeratePlays 3" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(3)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 4
		result.depthFirstTraverse.size shouldBe 29
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		writer.spilled shouldBe 546
	}

	it should "enumeratePlays 4" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(4)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 4
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		writer.spilled shouldBe 1086
		result.depthFirstTraverse.size shouldBe 53
	}

	it should "enumeratePlays 5" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(5)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 4
		val writer = MockWriter(16384)
		result.output(Output(writer)).close()
		writer.spilled shouldBe 2838
		result.depthFirstTraverse.size shouldBe 125
	}

	it should "enumeratePlays 8" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)
		def success(s: State): Boolean = s.tricks.ns >= 2

		def failure(s: State): Boolean = s.tricks.ew >= 1

		val result = target.enumeratePlays(9)(success, failure)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 21
	}

	it should "enumeratePlays 9" in {
		val deal = Deal("test", 2L)
		val whist = Whist(deal, 0)
		val target = Tree(whist)
		def success(s: State): Boolean = s.tricks.ns >= 3

		def failure(s: State): Boolean = s.tricks.ew >= 1

		val result = target.enumeratePlays(13)(success, failure)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 55
		// FIXME
		states foreach { s => println(s"${s.trick} ${s.tricks}") }
	}

}
