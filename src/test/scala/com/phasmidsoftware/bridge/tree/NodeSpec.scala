package com.phasmidsoftware.bridge.tree

import com.phasmidsoftware.output.{MockWriter, Output}
import org.scalatest.{FlatSpec, Matchers}

class NodeSpec extends FlatSpec with Matchers {

	behavior of "Node"

	it should "unit 1" in {
		val target = MockNode(1)
		val result = target.unit(2)
		result.t shouldBe 2
		result.children shouldBe Nil
	}

	it should "unit 2" in {
		val target = MockNode(1)
		val result = target.unit(2, terminal = false, Seq(MockNode(3)))
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
			case MockNode(1, false, Seq(MockNode(3, false, Nil))) =>
		}
	}

	it should "replace 2" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.replace(three, MockNode(4))
		result should matchPattern {
			case MockNode(1, false, Seq(MockNode(2, _, Seq(MockNode(4, _, Nil))))) =>
		}
	}

	it should "append 1" in {
		val two = MockNode(2)
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(two, MockNode(3))
		result should matchPattern {
			case MockNode(1, false, Seq(MockNode(2, false, Seq(MockNode(3, false, Nil))))) =>
		}
	}

	it should "append 2" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(three, MockNode(4))
		result should matchPattern {
			case MockNode(1, false, Seq(MockNode(2, false, Seq(MockNode(3, false, Seq(MockNode(4, false, Nil))))))) =>
		}
	}

	it should "apply 3" in {
		val two = MockNode(2)
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(two, 3)
		result should matchPattern {
			case MockNode(1, false, Seq(MockNode(2, false, Seq(MockNode(3, false, Nil))))) =>
		}
	}

	it should "append 4" in {
		val three = MockNode(3)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(two))
		val result: Node[Int] = target.append(three, 4)
		result should matchPattern {
			case MockNode(1, false, Seq(MockNode(2, false, Seq(MockNode(3, false, Seq(MockNode(4, false, Nil))))))) =>
		}
	}

	it should "unapply" in {
		MockNode.unapply(MockNode(1, Nil)) should matchPattern { case Some((1, false, Nil)) => }
		MockNode.unapply(MockNode(1, Seq(MockNode(2)))) should matchPattern { case Some((1, false, Seq(MockNode(2, false, Nil)))) => }
	}

	behavior of "expand"
	it should "work 1" in {
		val target = MockNode(1)
		trait SuccessorsInt extends Successors[Int] {
			def successors(t: Int): Option[Seq[Int]] = Some(Seq(t + 1))
		}
		implicit object SuccessorsInt extends SuccessorsInt
		val expansion: Option[Node[Int]] = target.expand(2)
		expansion match {
			case Some(n) => n.depthFirstTraverse shouldBe List(3, 2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with success condition (number 3 reached)" in {
		val target = MockNode(1)
		trait SuccessorsInt extends Successors[Int] {
			def successors(t: Int): Option[Seq[Int]] = if (t == 3) None else Some(Seq(t + 1, t + 2))
		}
		implicit object SuccessorsInt extends SuccessorsInt
		val expansion: Option[Node[Int]] = target.expand(10)
		expansion match {
			case Some(n) => n.depthFirstTraverse shouldBe List(3, 4, 3, 2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with failure condition (number 3 or higher reached)" in {
		val target = MockNode(1)
		trait SuccessorsInt extends Successors[Int] {
			def successors(t: Int): Option[Seq[Int]] = if (t >= 3) Some(Nil) else Some(Seq(t + 1, t + 2))
		}
		implicit object SuccessorsInt extends SuccessorsInt
		val expansion: Option[Node[Int]] = target.expand(10)
		expansion match {
			case Some(n) => n.depthFirstTraverse shouldBe List(3, 4, 3, 2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with both success and failure conditions" in {
		val target = AltMockNode("1")
		trait SuccessorsString extends Successors[String] {
			def successors(t: String): Option[Seq[String]] = if (t == "1.3.1.2") None else if (t.contains("1.2.3")) Some(Nil) else Some(Seq(t + ".1", t + ".2", t + ".3"))
		}
		implicit object SuccessorsString extends SuccessorsString
		val expansion: Option[Node[String]] = target.expand(8)
		expansion match {
			case Some(n) => n.depthFirstTraverse.size shouldBe 4693
			case None => fail("None returned")
		}
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

case class MockNode(t: Int, isTerminal: Boolean, children: Seq[Node[Int]]) extends Node[Int] {
	def unit(t: Int, terminal: Boolean, tns: Seq[Node[Int]]): Node[Int] = MockNode(t, terminal, tns)
}

object MockNode {
	def apply(t: Int, children: Seq[Node[Int]]): MockNode = apply(t, isTerminal = false, children)

	def apply(t: Int): MockNode = apply(t, Nil)
}

case class AltMockNode(t: String, isTerminal: Boolean, children: Seq[Node[String]]) extends Node[String] {
	def unit(t: String, terminal: Boolean, tns: Seq[Node[String]]): Node[String] = AltMockNode(t, terminal, tns)
}

object AltMockNode {
	def apply(t: String, children: Seq[Node[String]]): AltMockNode = apply(t, isTerminal = false, children)

	def apply(t: String): AltMockNode = apply(t, Nil)
}
