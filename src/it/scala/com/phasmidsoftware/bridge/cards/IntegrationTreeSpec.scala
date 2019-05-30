package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

class IntegrationTreeSpec extends FlatSpec with Matchers {

	def success(n: StateNode): Boolean = false

	behavior of "expand"

	private val deal2 = Deal("test", 2L)
	private val whist = Whist(deal2, 0)

	it should "go to level 12" in {
		Tree(whist).expand(12)(_ => false, _ => false).depthFirstTraverse.size shouldBe 47717
	}

	ignore should "go to level 16" in {
		Tree(whist).expand(16)(_ => false, _ => false).depthFirstTraverse.size shouldBe 1214963
	}

	it should "go to level 16 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(16)(_.tricks.ns >= 3, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 36
	}

	it should "go to level 20 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(20)(_.tricks.ns >= 3, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 36
	}

	it should "go to level 24 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(24)(_.tricks.ns >= 4, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 42
	}

	it should "go to level 28 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(28)(_.tricks.ns >= 5, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 59
	}

	it should "go to level 32 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(32)(_.tricks.ns >= 6, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 70
	}

	it should "go to level 36 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(36)(_.tricks.ns >= 7, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states foreach (s => println(s"${s.trick}, ${s.tricks}"))
		states.size shouldBe 82
	}

	it should "go to level 40 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(40)(_.tricks.ns >= 8, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 91
	}

	it should "go to level 40 with short alternative circuit" in {
		val target = Tree(whist)
		val result = target.expand(40)(_.tricks.ns >= 7, _.tricks.ew > 3)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 82
	}

	it should "go to level 44 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(44)(_.tricks.ns >= 9, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 105
	}

	it should "go to level 48 with short circuit" in {
		val target = Tree(whist)
		val result = target.expand(48)(_.tricks.ns >= 9, _.tricks.ew > 3)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 105
	}

	it should "go through all levels with short circuit based on 3NT" in {
		val target = Tree(whist)
		val result = target.enumerateNoTrumpPlaysNS(9)
		val states: Seq[State] = result.depthFirstTraverse
		states foreach (s => println(s"${s.trick}, ${s.tricks}"))
		states.size shouldBe 105
	}
}
