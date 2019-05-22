package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

class IntegrationTreeSpec extends FlatSpec with Matchers {

	def success(n: TreeNode): Boolean = false

	behavior of "enumeratePlays"
	it should "go to level 12" in {
		Tree(Deal("test", 2L)).enumeratePlays(12)(success).depthFirstTraverse.size shouldBe 25779
	}

	it should "go to level 16" in {
		Tree(Deal("test", 2L)).enumeratePlays(16)(success).depthFirstTraverse.size shouldBe 1214963
	}

	it should "go to level 16 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		def success(n: TreeNode): Boolean = n.state.tricks.ns >= 3 || n.state.tricks.ew >= 2

		val result = target.enumeratePlays(16)(success)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 247335
	}

	ignore should "go to level 20 with short circuit" in {
		val deal = Deal("test", 2L)
		val target = Tree(deal)

		def success(n: TreeNode): Boolean = n.state.tricks.ns >= 4 || n.state.tricks.ew >= 3

		val result = target.enumeratePlays(20)(success)
		val states: Seq[State] = result.depthFirstTraverse
		states.size shouldBe 247335
	}

}
