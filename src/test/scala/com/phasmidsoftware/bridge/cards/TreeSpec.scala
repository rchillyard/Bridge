package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

	behavior of "Tree"

	it should "apply" in {
		val deal = Deal("test", 0L)
		val trick = Trick(0, Nil, 0, Spades)
		val root = TreeNode(State(deal, trick, Tricks.zero), done = false, Nil)
		val target = Tree(root)
		target.root.state.deal shouldBe deal
		target.root.state.trick shouldBe trick
		target.root shouldBe root
	}

	it should "chooseLead" in {
		val deal = Deal("test", 0L)
		val state = State(deal)
		val target = Tree(state)
		val result: Seq[CardPlay] = state.chooseLead(0)
		result.size shouldBe 3
		result.head shouldBe CardPlay(deal, 0, Hearts, 2)
		result(1) shouldBe CardPlay(deal, 0, Hearts, 5)
		result.last shouldBe CardPlay(deal, 0, Hearts, 10)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		println(writer.spillway)
		writer.spilled shouldBe 9
	}

	it should "output" in {
		val target = Tree(Deal("test", 0L))
		val writer = MockWriter()
		target.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 9
	}

	it should "enumerateLeads 1" in {
		val deal = Deal("test", 2L)
		val state = State(deal)
		// Figure out all the possible leads from the North's longest and strongest suit.
		// Bear in mind that we consider all cards from a "sequence" equivalent.
		val ss: Seq[State] = state.enumerateLeads(0, 0)
		ss.size shouldBe 4
		ss.head should matchPattern { case State(_, _, _) => }
		ss.head.trick.toString shouldBe "T0 lead=0: H {Play: 0 HK}"
		ss.tail.head.trick.toString shouldBe "T0 lead=0: H {Play: 0 H9}"
		ss.init.last.trick.toString shouldBe "T0 lead=0: H {Play: 0 H5}"
		ss.last.trick.toString shouldBe "T0 lead=0: H {Play: 0 H2}"
	}

	it should "enumerateLeads 2" in {
		val deal = Deal("test", 2L)
		val hands = deal.hands
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick = Trick.create(0, 0, Spades, CardPlay(deal, 0, Spades, priority1S), CardPlay(deal, 1, Spades, priority2S), CardPlay(deal, 2, Spades, priority3S), CardPlay(deal, 3, Spades, priority4S))
		val deal2 = deal.playAll(trick)
		trick.winner match {
			case Some(winner) =>
				val state = State(deal2, trick, Tricks(0, 0).increment(trick))
				val ss: Seq[State] = state.enumerateLeads(winner, trick.index + 1)
				ss.size shouldBe 4
				ss.head should matchPattern { case State(_, _, _) => }
				ss.head.trick.toString shouldBe "T1 lead=0: H {Play: 0 HK}"
				ss.tail.head.trick.toString shouldBe "T1 lead=0: H {Play: 0 H9}"
				ss.init.last.trick.toString shouldBe "T1 lead=0: H {Play: 0 H5}"
				ss.last.trick.toString shouldBe "T1 lead=0: H {Play: 0 H2}"
			case None => fail("no winner")
		}

	}

	it should "enumerateFollows" in {
		val deal = Deal("test", 2L)
		val state = State(deal)
		val ss = state.enumerateFollows
		ss.size shouldBe 2
		ss.head.trick.toString shouldBe "T0 lead=0: S {Play: 0 SK}"
		ss.last.trick.toString shouldBe "T0 lead=0: S {Play: 0 ST}"
	}

	it should "enumeratePlays 1" in {
		val target = Tree(Deal("test", 2L))

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(1)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 2
		val writer = MockWriter()
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 42
		result.depthFirstTraverse.size shouldBe 3
	}

	it should "enumeratePlays 2" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(2)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 2
		result.depthFirstTraverse.size shouldBe 7
		val writer = MockWriter()
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 116
	}

	it should "enumeratePlays 3" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(3)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 2
		result.depthFirstTraverse.size shouldBe 15
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 280
	}

	it should "enumeratePlays 4" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(4)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 2
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 816
		result.depthFirstTraverse.size shouldBe 39
	}

	it should "enumeratePlays 5" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		def alwaysFalse(n: State): Boolean = false

		val result = target.enumeratePlays(5)(alwaysFalse, alwaysFalse)
		result.children.size shouldBe 2
		val writer = MockWriter(16384)
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 3000
		result.depthFirstTraverse.size shouldBe 129
	}

	it should "enumeratePlays 8" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)
		def success(s: State): Boolean = s.tricks.ns >= 2

		def failure(s: State): Boolean = s.tricks.ew >= 1

		val result = target.enumeratePlays(9)(success, failure)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 23
		states foreach {
			s =>
				if (s.trick.isComplete)
					println(s"${s.trick} ${s.tricks}")
		}
	}

	it should "enumeratePlays 9" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)
		def success(s: State): Boolean = s.tricks.ns >= 3

		def failure(s: State): Boolean = s.tricks.ew >= 1

		val result = target.enumeratePlays(13)(success, failure)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 33
	}

}
