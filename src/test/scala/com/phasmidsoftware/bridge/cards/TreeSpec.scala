package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

	behavior of "Tree"

	it should "apply" in {
		val deal = Deal("test", 0L)
		val root = TrickNode(Trick(Nil, 0, Spades), Nil)
		val target = Tree(deal, root)
		target.deal shouldBe deal
		target.root shouldBe root
	}

	it should "chooseLead" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		val result: Seq[CardPlay] = target.chooseLead(0)
		result.size shouldBe 3
		result.head shouldBe CardPlay(0, Hearts, 2)
		result(1) shouldBe CardPlay(0, Hearts, 5)
		result.last shouldBe CardPlay(0, Hearts, 10)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		println(writer.spillway)
		writer.spilled shouldBe 8

	}

	it should "output" in {
		val target = Tree(Deal("test", 0L))
		val writer = MockWriter()
		target.output(Output(writer)).close()
		println(writer.spillway)
		writer.spilled shouldBe 8
	}

	it should "enumeratePlays 1" in {
		val deal = Deal("test", 0L)
		println(deal)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(Nil, 0, Spades)
		val to: Option[TrickNode] = target.enumeratePlays(TrickNode(trick, Nil), 1)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter()
		result.output(Output(writer)).close()
		println(writer.spillway)
		writer.spilled shouldBe 61
	}

	it should "enumeratePlays 2" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(Nil, 0, Spades)
		val to: Option[TrickNode] = target.enumeratePlays(TrickNode(trick, Nil), 2)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter()
		result.output(Output(writer)).close()
		println(writer.spillway)
		writer.spilled shouldBe 419
	}

	it should "enumeratePlays 3" in {
		val deal = Deal("test", 0L)
		val target = Tree(deal)
		// Arbitrarily start play with North leading a spade.
		val trick = Trick(Nil, 0, Spades)
		val to: Option[TrickNode] = target.enumeratePlays(TrickNode(trick, Nil), 3)
		to should matchPattern { case Some(_) => }
		val result = to.get
		result.children.size shouldBe 2
		val writer = MockWriter(8192)
		result.output(Output(writer)).close()
		println(writer.spillway)
		writer.spilled shouldBe 2427
	}
}
