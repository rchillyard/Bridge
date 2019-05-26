package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

class IntegrationTreeSpec extends FlatSpec with Matchers {

	def success(n: StateNode): Boolean = false

	behavior of "enumeratePlays"
	it should "go to level 12" in {
		Tree(Deal("test", 2L)).enumeratePlays(12)(_ => false, _ => false).depthFirstTraverse.size shouldBe 25779
	}

	ignore should "go to level 16" in {
		Tree(Deal("test", 2L)).enumeratePlays(16)(_ => false, _ => false).depthFirstTraverse.size shouldBe 1214963
	}

	it should "go to level 16 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(16)(_.tricks.ns >= 3, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 33
	}

	it should "go to level 20 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(20)(_.tricks.ns >= 3, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 33
	}

	it should "go to level 24 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(24)(_.tricks.ns >= 4, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 48
	}

	it should "go to level 28 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(28)(_.tricks.ns >= 5, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 65
	}

	it should "go to level 32 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(32)(_.tricks.ns >= 6, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 71
	}

	it should "go to level 36 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(36)(_.tricks.ns >= 7, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 82
	}

	it should "go to level 40 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(40)(_.tricks.ns >= 8, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 183
	}

	it should "go to level 40 with short alternative circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(40)(_.tricks.ns >= 7, _.tricks.ew > 3)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 82
	}

	it should "go to level 44 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(44)(_.tricks.ns >= 9, _.tricks.ew > 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 298
	}

	it should "go to level 48 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(48)(_.tricks.ns >= 9, _.tricks.ew > 3)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 2266
	}

	it should "go through all levels with short circuit based on 3NT" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumerateNoTrumpPlays(9)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 120
		//		states foreach (s => println(s"${s.trick}, ${s.tricks}"))
	}
}
