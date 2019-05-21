package com.phasmidsoftware.bridge.tree

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class NodeSpec extends FlatSpec with Matchers {

	case class MockNode(t: Int, children: Seq[Node[Int]] = Nil) extends Node[Int] {
		def unit(t: Int, tns: Seq[Node[Int]]): Node[Int] = MockNode(t, tns)
	}

	behavior of "Node"

	it should "unit 1" in {
		val target = MockNode(1)
		val result = target.unit(2)
		result.t shouldBe 2
		result.children shouldBe Nil
	}

	it should "unit 2" in {
		val target = MockNode(1)
		val result = target.unit(2, Seq(MockNode(3)))
		result.t shouldBe 2
		result.children shouldBe Seq(MockNode(3))
	}

	it should "output 1" in {
		val target = MockNode(1)
		val writer = MockWriter()
		target.output(Output(writer)).close()
		writer.spillway shouldBe "1"
	}

	it should "output 2" in {
		val target = MockNode(1, Seq(MockNode(2, Seq(MockNode(3)))))
		val writer = MockWriter()
		target.output(Output(writer)).close()
		// TODO fix this -- there should be no space before the newlines
		writer.spillway shouldBe "1 \n  2 \n    3"
	}

	it should "output 3" in {
		val target = MockNode(1, Seq(MockNode(2, Seq(MockNode(3, Seq(MockNode(4)))))))
		val writer = MockWriter()
		target.output(Output(writer)).close()
		// TODO fix this -- there should be no space before the newlines
		writer.spillway shouldBe "1 \n  2 \n    3 \n      4"
	}

	it should "replace 1" in {
		val two = MockNode(2)
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.replace(two, MockNode(3))
		result should matchPattern {
			case MockNode(1, Seq(MockNode(3, Nil))) =>
		}
	}

	it should "replace 2" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.replace(three, MockNode(4))
		result should matchPattern {
			case MockNode(1, Seq(MockNode(2, Seq(MockNode(4, _))))) =>
		}
	}

	it should "append 1" in {
		val two = MockNode(2)
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(two, MockNode(3))
		result should matchPattern {
			case MockNode(1, Seq(MockNode(2, Seq(MockNode(3, Nil))))) =>
		}
	}

	it should "append 2" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(three, MockNode(4))
		result should matchPattern {
			case MockNode(1, Seq(MockNode(2, Seq(MockNode(3, Seq(MockNode(4, _))))))) =>
		}
	}

	it should "apply 3" in {
		val two = MockNode(2)
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(two, 3)
		result should matchPattern {
			case MockNode(1, Seq(MockNode(2, Seq(MockNode(3, Nil))))) =>
		}
	}

	it should "append 4" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(three, 4)
		result should matchPattern {
			case MockNode(1, Seq(MockNode(2, Seq(MockNode(3, Seq(MockNode(4, _))))))) =>
		}
	}

	it should "unapply" in {
		MockNode.unapply(MockNode(1, Nil)) should matchPattern { case Some((1, Nil)) => }
		MockNode.unapply(MockNode(1, Seq(MockNode(2)))) should matchPattern { case Some((1, Seq(MockNode(2, Nil)))) => }
	}

	behavior of "dfs"
	it should "work for sum" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result = target.dfs(0)(_ + _)
		result shouldBe 6
	}

	it should "work for list" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result = target.dfs(List[Int]())((z, t) => t +: z)
		result shouldBe List(3, 2, 1)
	}

	behavior of "depthFirstTraverse"
	it should "work" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result = target.depthFirstTraverse
		result shouldBe List(3, 2, 1)
	}

}
