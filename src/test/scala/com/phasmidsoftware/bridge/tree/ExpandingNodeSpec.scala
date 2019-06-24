/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.tree

import com.phasmidsoftware.output.Loggable.{LoggableInt, LoggableString}
import org.scalatest.{FlatSpec, Matchers}

//noinspection ScalaStyle
class ExpandingNodeSpec extends FlatSpec with Matchers {


	behavior of "expand"
	it should "work 1" in {
		val target = MockNode(1)
		trait ExpandableInt$ extends Expandable[Int] {
			def decide(t: Int): Option[Boolean] = None

			def successors(t: Int): Seq[Int] = Seq(t + 1)
		}
		implicit object ExpandableInt extends ExpandableInt$ {
			def canDecide(t: Int, to: Option[Int]): Boolean = false
		}
		val expansion: Option[Node[Int]] = target.expand(2)
		expansion match {
			case Some(n) => n.depthFirstTraverse shouldBe List(2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with success condition (number 3 reached)" in {
		val target = AltMockNode1(1)
		trait SuccessorsInt extends Expandable[Int] {
			def decide(t: Int): Option[Boolean] =
				if (t == 3) Some(true) else None

			def successors(t: Int): Seq[Int] = Seq(t + 1, t + 2)
		}
		implicit object SuccessorsInt extends SuccessorsInt {
			def canDecide(t: Int, to: Option[Int]): Boolean = false
		}
		val expansion: Option[Node[Int]] = target.expand(10)
		expansion match {
			case Some(n) => n.depthFirstTraverse shouldBe List(3, 4, 3, 2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with noExpandFunction condition (number 3 or higher reached)" in {
		val target = AltMockNode2(1)
		val expansion: Option[Node[Int]] = target.expand(10)
		expansion match {
			case Some(n) => n.depthFirstTraverse shouldBe List(2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with result n(AltNode)" in {
		val target = AltMockNodeS("1")
		val expansion: Option[Node[String]] = target.expand(8)
		expansion match {
			case Some(n) => n.depthFirstTraverse.size shouldBe 1568
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

trait ExpandableInt extends Expandable[Int] {
	def decide(t: Int): Option[Boolean] = if (t == 3) Some(true) else None

	def canDecide(t: Int, to: Option[Int]): Boolean = true

	def successors(t: Int): Seq[Int] = ???
}

case class MockNode(override val t: Int, override val decided: Option[Boolean], override val children: Seq[ExpandingNode[Int]]) extends
	ExpandingNode[Int](t, decided, children)(new Expandable[Int] {
		def decide(t: Int): Option[Boolean] = None

		def canDecide(t: Int, to: Option[Int]): Boolean = true

		def successors(t: Int): Seq[Int] = Seq(t + 1)
	}, LoggableInt) {

	def unit(_t: Int, decide: Option[Boolean], tns: Seq[Node[Int]]): ExpandingNode[Int] = MockNode(_t, decide, tns.asInstanceOf[Seq[ExpandingNode[Int]]])
}

object MockNode {
	def apply(t: Int, children: Seq[ExpandingNode[Int]]): MockNode = MockNode(t, None, children)

	def apply(t: Int): MockNode = apply(t, Nil)
}

case class AltMockNode1(override val t: Int, override val decided: Option[Boolean], override val children: Seq[ExpandingNode[Int]]) extends
	ExpandingNode[Int](t, decided, children)(new Expandable[Int] {
		def decide(t: Int): Option[Boolean] = if (t >= 3) Some(true) else None

		def canDecide(t: Int, to: Option[Int]): Boolean = true

		def successors(t: Int): Seq[Int] = Seq(t + 1, t + 2)
	}, LoggableInt) {

	def unit(_t: Int, decide: Option[Boolean], tns: Seq[Node[Int]]): ExpandingNode[Int] = AltMockNode1(_t, decide, tns.asInstanceOf[Seq[ExpandingNode[Int]]])

}

object AltMockNode1 {
	def apply(t: Int, children: Seq[ExpandingNode[Int]]): AltMockNode1 = AltMockNode1(t, None, children)

	def apply(t: Int): AltMockNode1 = apply(t, Nil)
}

case class AltMockNode2(override val t: Int, override val decided: Option[Boolean], override val children: Seq[ExpandingNode[Int]]) extends
	ExpandingNode[Int](t, decided, children)(new Expandable[Int] {

		def decide(t: Int): Option[Boolean] = None

		def canDecide(t: Int, to: Option[Int]): Boolean = t < 3

		def successors(t: Int): Seq[Int] = Seq(t + 1, t + 2)
	}, LoggableInt) {

	def unit(_t: Int, decide: Option[Boolean], tns: Seq[Node[Int]]): ExpandingNode[Int] = AltMockNode2(_t, decide, tns.asInstanceOf[Seq[ExpandingNode[Int]]])
}

object AltMockNode2 {
	def apply(t: Int, children: Seq[ExpandingNode[Int]]): AltMockNode2 = AltMockNode2(t, None, children)

	def apply(t: Int): AltMockNode2 = apply(t, Nil)
}

trait ExpandableString extends Expandable[String] {
	def decide(t: String): Option[Boolean] = ???

	def canDecide(t: String, to: Option[String]): Boolean = ???

	def successors(t: String): Seq[String] = ???
}

case class AltMockNodeS(override val t: String, override val decided: Option[Boolean], override val children: Seq[AltMockNodeS])
	extends ExpandingNode[String](t, decided, children)(new Expandable[String] {

		def canDecide(t: String, to: Option[String]): Boolean = !t.contains("1.2.3")

		def decide(t: String): Option[Boolean] = if (t == "1.3.1.2") Some(true) else None

		def successors(t: String): Seq[String] = Seq(t + ".1", t + ".2", t + ".3")

	}, LoggableString) {
	def unit(t: String, decided: Option[Boolean], tns: Seq[Node[String]]): ExpandingNode[String] =
		AltMockNodeS(t, decided, tns.asInstanceOf[Seq[AltMockNodeS]])

}

object AltMockNodeS {
	//	def apply(t: String, children: Seq[ExpandingNode[String]]): AltMockNodeS = apply(t, children)

	def apply(t: String): AltMockNodeS = apply(t, None, Nil)

	implicit object ExpandableString extends ExpandableString

	val ExpString: Expandable[String] = ExpandableString
}
