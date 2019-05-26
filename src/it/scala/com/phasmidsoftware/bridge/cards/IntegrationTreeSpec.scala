package com.phasmidsoftware.bridge.cards

class IntegrationTreeSpec extends FlatSpec with Matchers {

	def success(n: TreeNode): Boolean = false

	behavior of "enumeratePlays"
	it should "go to level 12" in {
		Tree(Deal("test", 2L)).enumeratePlays(12)(_ => false, _ => false).depthFirstTraverse.size shouldBe 25779
	}

	it should "go to level 16" in {
		Tree(Deal("test", 2L)).enumeratePlays(16)(_ => false, _ => false).depthFirstTraverse.size shouldBe 1214963
	}

	it should "go to level 17 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(17)(_.tricks.ns >= 3, _.tricks.ew >= 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 33
	}

	it should "go to level 21 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(21)(_.tricks.ns >= 3, _.tricks.ew >= 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 33
	}

	it should "go to level 25 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		val result = target.enumeratePlays(25)(_.tricks.ns >= 4, _.tricks.ew >= 2)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 48
	}

}
