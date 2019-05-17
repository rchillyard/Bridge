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
		println(writer.spillway)
		writer.spilled shouldBe 3

	}

	it should "output" in {
		val target = Tree(Deal("test", 0L))
		val writer = MockWriter()
		target.output(Output(writer)).close()
		println(writer.spillway)
		writer.spilled shouldBe 3
	}

	it should "enumeratePlays 1" in {
		val deal = Deal("test", 0L)
		println(deal)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(0, Nil, 0, Spades)
		val to: Option[TreeNode] = target.enumeratePlays(TreeNode(State(deal, trick), Nil), 1)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter()
		result.output(Output(writer)).close()
		println(writer.spillway)
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
		println(writer.spillway)
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
		println(writer.spillway)
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
		println(writer.spillway)
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
		println(writer.spillway)
		writer.spilled shouldBe 6198
	}
}
