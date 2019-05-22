package com.phasmidsoftware.bridge.cards

import org.scalatest.{FlatSpec, Matchers}

class IntegrationTreeSpec extends FlatSpec with Matchers {

	behavior of "enumeratePlays"
	it should "go to level 12" in {
		Tree(Deal("test", 2L)).enumeratePlays(12).depthFirstTraverse.size shouldBe 25779
	}

	it should "go to level 16" in {
		Tree(Deal("test", 2L)).enumeratePlays(16).depthFirstTraverse.size shouldBe 1214963
	}

}
