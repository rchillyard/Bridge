package com.phasmidsoftware.bridge.cards

class IntegrationTreeSpec extends FlatSpec with Matchers {

	behavior of "enumeratePlays"
	it should "go to level 12" in {
		Tree(Deal("test", 0L)).enumeratePlays(12).depthFirstTraverse.size shouldBe 58633
	}

	it should "go to level 16" in {
		Tree(Deal("test", 0L)).enumeratePlays(16).depthFirstTraverse.size shouldBe 1504177
	}

}
