package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

	behavior of "Tree"

	it should "apply" in {
		val deal = Deal("test", 0L)
		val trick = Trick(0, Nil, 0, Spades)
		val root = TreeNode(State(deal, trick), Nil)
		val target = Tree(root)
		target.root.state.deal shouldBe deal
		target.root.state.trick shouldBe trick
		target.root shouldBe root
	}

	it should "chooseLead" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		val result: Seq[CardPlay] = target.chooseLead(deal, 0)
		result.size shouldBe 3
		result.head shouldBe CardPlay(deal, 0, Hearts, 2)
		result(1) shouldBe CardPlay(deal, 0, Hearts, 5)
		result.last shouldBe CardPlay(deal, 0, Hearts, 10)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 3

	}

	it should "output" in {
		val target = Tree(Deal("test", 0L))
		val writer = MockWriter()
		target.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 3
	}

	it should "enumerateLeads 1" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Figure out all the possible leads from the North's longest and strongest suit.
		// Bear in mind that we consider all cards from a "sequence" equivalent.
		val ss: Seq[State] = target.enumerateLeads(deal, 0, 0)
		ss.size shouldBe 3
		ss.head should matchPattern { case State(_, _) => }
		ss.head.trick.toString shouldBe "T0 lead=0: H {Play: 0 HQ}"
		ss.tail.head.trick.toString shouldBe "T0 lead=0: H {Play: 0 H9}"
		ss.last.trick.toString shouldBe "T0 lead=0: H {Play: 0 H4}"
	}

	it should "enumerateLeads 2" in {
		val deal = Deal("test", 0L)
		val hands = deal.hands
		val target = Tree(deal)
		val Seq(priority1S, priority2S, priority3S, priority4S) = hands map (_.holdings(Spades).sequences.last.priority)
		val trick = Trick.create(0, 0, Spades, CardPlay(deal, 0, Spades, priority1S), CardPlay(deal, 1, Spades, priority2S), CardPlay(deal, 2, Spades, priority3S), CardPlay(deal, 3, Spades, priority4S))
		val deal2 = deal.playAll(trick)
		trick.winner match {
			case Some(winner) =>
				val ss: Seq[State] = target.enumerateLeads(deal2, 1, winner)
				ss.size shouldBe 3
				ss.head should matchPattern { case State(_, _) => }
				ss.head.trick.toString shouldBe "T1 lead=2: S {Play: 2 SA}"
				ss.tail.head.trick.toString shouldBe "T1 lead=2: S {Play: 2 SJ}"
				ss.last.trick.toString shouldBe "T1 lead=2: S {Play: 2 S8}"
			case None => fail("no winner")
		}

	}

	it should "enumerateFollows" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(0, Nil, 0, Spades)
		val ss: Seq[State] = target.enumerateFollows(deal, State(deal, trick))
		ss.size shouldBe 2
		ss.head.trick.toString shouldBe "T0 lead=0: S {Play: 0 S9}"
		ss.last.trick.toString shouldBe "T0 lead=0: S {Play: 0 S5}"
	}

	it should "enumeratePlays 1" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(0, Nil, 0, Spades)
		val to: Option[TreeNode] = target.enumeratePlays(TreeNode(State(deal, trick), Nil), 1)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter()
		result.output(Output(writer)).close()
		writer.spilled shouldBe 24
	}

	it should "enumeratePlays 2" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(0, Nil, 0, Spades)
		val to: Option[TreeNode] = target.enumeratePlays(TreeNode(State(deal, trick), Nil), 2)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter()
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 122
	}

	it should "enumeratePlays 3" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(0, Nil, 0, Spades)
		val to: Option[TreeNode] = target.enumeratePlays(TreeNode(State(deal, trick), Nil), 3)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 578
	}

	it should "enumeratePlays 4" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(0, Nil, 0, Spades)
		val to: Option[TreeNode] = target.enumeratePlays(TreeNode(State(deal, trick), Nil), 4)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 1634
	}

	it should "enumeratePlays 5" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(0, Nil, 0, Spades)
		val to: Option[TreeNode] = target.enumeratePlays(TreeNode(State(deal, trick), Nil), 5)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter(16384)
		result.output(Output(writer)).close()
		//		println(writer.spillway)
		writer.spilled shouldBe 5334
	}
}
