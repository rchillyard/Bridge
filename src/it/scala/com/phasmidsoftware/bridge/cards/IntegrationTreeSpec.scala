package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

class IntegrationTreeSpec extends FlatSpec with Matchers {

	def success(n: StateNode): Boolean = false

	behavior of "expand"

	private val deal2 = Deal("test", 2L)
	private val whist = Whist(deal2, 0)

	it should "go to level 12" in {
		Tree(whist).expand(12).depthFirstTraverse.size shouldBe 23021
	}

	// NOTE: the following test actually works but it takes a long time!
	ignore should "go to level 16" in {
		Tree(whist).expand(16).depthFirstTraverse.size shouldBe 730493
	}

	it should "go to level 16 with short circuit" in {
		val target = Tree(whist)
		//		val result = target.expand(16)(_.tricks.ns >= 3, _.tricks.ew > 2)
		val result = target.expand(16)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 31
	}

	it should "go to level 20 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(20)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 31
	}

	it should "go to level 24 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(24)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 41
	}

	it should "go to level 28 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(28)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 51
	}

	it should "go to level 32 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(32)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 61
	}

	it should "go to level 36 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(36)
		val states: Seq[State] = result.depthFirstTraverse
		//		states foreach (s => println(s"${s.trick}, ${s.tricks}"))
		states.size shouldBe 71
	}

	it should "go to level 40 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(40)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 81
	}

	it should "go to level 40 with short alternative circuit" in {
		val target = Tree(whist)
		val result = target.expand(40)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 71
	}

	it should "go to level 44 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(44)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 91
	}

	it should "go to level 48 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(48)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 91
	}

	it should "go through all levels with short circuit based on 3NT" in {
		val target = Tree(whist)
		val result = target.enumerateNoTrumpPlaysNS(9)
		val states: Seq[State] = result.depthFirstTraverse
		//		states foreach (s => println(s"${s.trick}, ${s.tricks}"))
		states.size shouldBe 91
	}
}
