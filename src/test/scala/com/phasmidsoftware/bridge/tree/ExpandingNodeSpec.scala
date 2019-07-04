/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.tree

import java.util.regex.Pattern

import com.phasmidsoftware.output.Loggable.{LoggableInt, LoggableString}
import com.phasmidsoftware.output.Output
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

//noinspection ScalaStyle
class ExpandingNodeSpec extends FlatSpec with Matchers with PrivateMethodTester {


	behavior of "expand"
	it should "work 1" in {
		val target = MockNode(1)
		trait ExpandableInt$ extends Expandable[Int] {
			def successors(t: Int): Seq[Int] = Seq(t + 1)
		}
		implicit object ExpandableInt extends ExpandableInt$ {
		}
		val expansion: Option[Node[Int]] = target.expand(None, 2)
		expansion match {
			case Some(n) => n.depthFirstTraverse shouldBe List(1)
			case None => fail("None returned")
		}
	}

	it should "work with success condition (number 3 reached)" in {
		val target = AltMockNode1(1)
		trait SuccessorsInt extends Expandable[Int] {
			def successors(t: Int): Seq[Int] = Seq(t + 1, t + 2)
		}
		implicit object SuccessorsInt extends SuccessorsInt {
		}
		val expansion: Option[Node[Int]] = target.expand(None, 10)
		expansion match {
			case Some(n) =>
				n.leaves shouldBe List(3, 4, 3)
				n.depthFirstTraverse shouldBe List(3, 4, 3, 2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with noExpandFunction condition (number 5 or higher reached)" in {
		val target = AltMockNode2(1)
		val expansion: Option[Node[Int]] = target.expand(None, 10)
		expansion match {
			case Some(n) =>
				n.leaves shouldBe List(5, 5, 5, 5, 5)
				n.depthFirstTraverse shouldBe List(5, 5, 4, 3, 5, 4, 5, 5, 4, 3, 2, 1)
			case None => fail("None returned")
		}
	}

	it should "work with AltMockNodeSimple" in {
		import com.phasmidsoftware.output.Flog._

		implicit val expandableString: Expandable[String] = new Expandable[String] {
			def successor(t: String, x: Int): String = (if (t.isEmpty) "" else t) + x

			def successors(t: String): Seq[String] = for (x <- 4 to 1 by -1) yield successor(t, x)
		}
		implicit val goalDrivenString: GoalDriven[String] = new GoalDriven[String] {
			private val goal = "42"

			def goalAchieved(t: String): Boolean = s"goalAchieved?: $t" !! (t endsWith goal)

			def goalOutOfReach(t: String, so: Option[String], moves: Int): Boolean =
				SubStringMatch.substringPrefix(moves, t, goal) > moves || (so match {
					case Some(s) => t.length >= s.length
					case None => false

				})
		}
		case class AltMockNodeSimple(override val t: String, override val so: Option[String], override val children: Seq[AltMockNodeSimple])
			extends ExpandingNode[String](t, so, children) {
			def unit(t: String, so: Option[String], tns: Seq[Node[String]]): ExpandingNode[String] = AltMockNodeSimple(t, so, tns.asInstanceOf[Seq[AltMockNodeSimple]])
		}
		val target = AltMockNodeSimple("", None, Nil)
		val expansion: Option[Node[String]] = target.expand(None, 2)

		expansion match {
			case Some(n) =>
				n.output(Output(System.out)).insertBreak.close()
				println(n.leaves)
				n.leaves.size shouldBe 1
				n.leaves.head shouldBe "42"
				n.depthFirstTraverse.size shouldBe 3
			case None => fail("None returned")
		}
	}

	it should "work with result n(AltNodeS) (4)" in {
		val target = AltMockNodeS("")
		val expansion: Option[Node[String]] = target.expand(None, 4)
		expansion match {
			case Some(n) =>
				n.leaves.size shouldBe 1
				n.depthFirstTraverse.size shouldBe 5
			case None => fail("None returned")
		}
	}

	it should "work with result n(AltNodeS) (5)" in {
		val target = AltMockNodeS("")
		val expansion: Option[Node[String]] = target.expand(None, 5)
		expansion match {
			case Some(n) =>
				n.output(Output(System.out)).insertBreak.close()
				println(n.leaves)
				n.leaves.size shouldBe 2
				n.leaves shouldBe List("1.1.3.1.2", "1.3.1.2")
				n.depthFirstTraverse.size shouldBe 9
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

	it should "work for list 2" in {
		val three = MockNode(3)
		val four = MockNode(4)
		val two = MockNode(2, Seq(three, four))
		val target = MockNode(1, Seq(two))
		val result = target.dfs(List[Int]())((z, t) => t +: z)
		result shouldBe List(4, 3, 2, 1)
	}

	it should "work for list 3" in {
		val three = MockNode(3)
		val four = MockNode(4)
		val two = MockNode(2, Seq(three))
		val target = MockNode(1, Seq(four, two))
		val result = target.dfs(List[Int]())((z, t) => t +: z)
		result shouldBe List(3, 2, 4, 1)
	}

	behavior of "AltNodeS"
	it should "find impossible false" in {
		AltMockNodeS.impossible("", 4) shouldBe false
		AltMockNodeS.impossible("1", 3) shouldBe false
		AltMockNodeS.impossible("1.3", 2) shouldBe false
		AltMockNodeS.impossible("1.3.1", 1) shouldBe false
		AltMockNodeS.impossible("1.3.1.2", 0) shouldBe false
		AltMockNodeS.impossible("1.2.3.4", 4) shouldBe false
	}
	it should "find impossible true" in {
		AltMockNodeS.impossible("", 0) shouldBe true
		AltMockNodeS.impossible("", 1) shouldBe true
		AltMockNodeS.impossible("", 2) shouldBe true
		AltMockNodeS.impossible("", 3) shouldBe true
		AltMockNodeS.impossible("2", 3) shouldBe true
		AltMockNodeS.impossible("1.3.3", 3) shouldBe true
		AltMockNodeS.impossible("1.2.3.4", 0) shouldBe true
		AltMockNodeS.impossible("1.2.3.4", 1) shouldBe true
		AltMockNodeS.impossible("1.2.3.4", 2) shouldBe true
		AltMockNodeS.impossible("1.2.3.4", 3) shouldBe true
	}

	behavior of "ExpandingNode"

	//	it should "expandAndReplace" in {
	//		val expandAndReplaceMethod = PrivateMethod[ExpandingNode[Int]]('expandAndReplace)
	//		val target = MockNode(1)
	//		trait ExpandableInt$ extends Expandable[Int] {
	//			def successors(t: Int): Seq[Int] = Seq(t + 1)
	//		}
	//		implicit object ExpandableInt extends ExpandableInt$ {
	//		}
	//		val result = target invokePrivate expandAndReplaceMethod(target, 1)
	//		result shouldBe ???
	//	}

	it should "replace" in {

	}

	it should "remove" in {

	}

	it should "output (1)" in {

	}

	it should "output (2)" in {

	}

	it should "decided" in {

	}

	it should "outputChildren" in {

	}

	it should "children" in {

	}

	it should "$plus$plus" in {

	}

	it should "outputValue" in {

	}

	it should "dfs" in {

	}

	it should "$colon$plus (1)" in {

	}

	it should "$colon$plus (2)" in {

	}

	it should "$colon$plus (3)" in {

	}

	it should "expand" in {

	}

	it should "unit (1)" in {

	}

	it should "unit (2)" in {

	}

	it should "unit (3)" in {

	}

	it should "t" in {

	}

	it should "append (1)" in {

	}

	it should "append (2)" in {

	}

	it should "append (3)" in {

	}

	it should "optionLoggable" in {

	}

	it should "sequenceLoggable" in {

	}

	it should "mapLoggable" in {

	}

	it should "valueToLog" in {

	}

	it should "toLog3" in {

	}

	it should "toLog4" in {

	}

	it should "toLog1" in {

	}

	it should "toLog2" in {

	}

	it should "expandingNodeLogger" in {

	}

}

case class MockNode(override val t: Int, override val so: Option[Int], override val children: Seq[ExpandingNode[Int]]) extends {
	private implicit val expandableInt: Expandable[Int] = (t: Int) => Seq(t + 1)
	private implicit val goalDrivenInt: GoalDriven[Int] = new GoalDriven[Int] {
		def goalAchieved(t: Int): Boolean = false

		def goalOutOfReach(t: Int, so: Option[Int], moves: Int): Boolean = false
	}
} with ExpandingNode[Int](t, so, children) {

	def unit(_t: Int, decide: Option[Int], tns: Seq[Node[Int]]): ExpandingNode[Int] = MockNode(_t, decide, tns.asInstanceOf[Seq[ExpandingNode[Int]]])
}

object MockNode {
	def apply(t: Int, children: Seq[ExpandingNode[Int]]): MockNode = MockNode(t, None, children)

	def apply(t: Int): MockNode = apply(t, Nil)
}

case class AltMockNode1(override val t: Int, override val so: Option[Int], override val children: Seq[ExpandingNode[Int]]) extends {
	private implicit val expandableInt: Expandable[Int] = (t: Int) => Seq(t + 1, t + 2)
	private implicit val goalDrivenInt: GoalDriven[Int] = new GoalDriven[Int] {
		def goalAchieved(t: Int): Boolean = t >= 3

		def goalOutOfReach(t: Int, so: Option[Int], moves: Int): Boolean = false
	}
} with ExpandingNode[Int](t, so, children) {

	def unit(_t: Int, so: Option[Int], tns: Seq[Node[Int]]): ExpandingNode[Int] = AltMockNode1(_t, so, tns.asInstanceOf[Seq[ExpandingNode[Int]]])

}

object AltMockNode1 {
	def apply(t: Int, children: Seq[ExpandingNode[Int]]): AltMockNode1 = AltMockNode1(t, None, children)

	def apply(t: Int): AltMockNode1 = apply(t, Nil)
}

case class AltMockNode2(override val t: Int, override val so: Option[Int], override val children: Seq[ExpandingNode[Int]]) extends {
	implicit private val expandableInt: Expandable[Int] = (t: Int) => Seq(t + 1, t + 2)
	private implicit val goalDrivenInt: GoalDriven[Int] = new GoalDriven[Int] {
		def goalAchieved(t: Int): Boolean = t == 5

		def goalOutOfReach(t: Int, so: Option[Int], moves: Int): Boolean = so match {
			case Some(s) => t > s
			case None => false
		}
	}
} with ExpandingNode[Int](t, so, children) {

	def unit(_t: Int, so: Option[Int], tns: Seq[Node[Int]]): ExpandingNode[Int] = AltMockNode2(_t, so, tns.asInstanceOf[Seq[ExpandingNode[Int]]])
}

object AltMockNode2 {
	def apply(t: Int, children: Seq[ExpandingNode[Int]]): AltMockNode2 = AltMockNode2(t, None, children)

	def apply(t: Int): AltMockNode2 = apply(t, Nil)
}

//trait ExpandableString extends Expandable[String] {
//	def successors(t: String): Seq[String] = ???
//}

case class AltMockNodeS(override val t: String, override val so: Option[String], override val children: Seq[AltMockNodeS])
	extends {
		private implicit val expandableString: Expandable[String] = new Expandable[String] {
			def successor(t: String, x: Int): String = (if (t.isEmpty) "" else t + ".") + x

			def successors(t: String): Seq[String] = for (x <- 1 to 3) yield successor(t, x)
		}
		private implicit val goalDrivenString: GoalDriven[String] = new GoalDriven[String] {
			def goalAchieved(t: String): Boolean = t endsWith AltMockNodeS.goal

			//			def goalOutOfReach(t: String, so: Option[String], moves: Int): Boolean = so match {
			//				case Some(s) => t.length >= s.length
			//				case None => AltMockNodeS.impossible(t, moves)
			//			}
			def goalOutOfReach(t: String, so: Option[String], moves: Int): Boolean = AltMockNodeS.impossible(t, moves) || (so match {
				case Some(s) => t.length >= s.length
				case None => false

			})
		}
	} with ExpandingNode[String](t, so, children) {
	def unit(t: String, so: Option[String], tns: Seq[Node[String]]): ExpandingNode[String] =
		AltMockNodeS(t, so, tns.asInstanceOf[Seq[AltMockNodeS]])

}

object AltMockNodeS {
	val goal = "1.3.1.2"
	private val patternDot = Pattern.compile(".", Pattern.LITERAL)

	private def abbreviate(t: String) = patternDot.matcher(t).replaceAll("")

	private val briefGoal = abbreviate(goal)

	def impossible(t: String, moves: Int): Boolean = SubStringMatch.substringPrefix(moves, abbreviate(t), briefGoal) > moves


	//	def apply(t: String, children: Seq[ExpandingNode[String]]): AltMockNodeS = apply(t, children)

	def apply(t: String): AltMockNodeS = apply(t, None, Nil)

//	implicit object ExpandableString extends ExpandableString
//
//	val ExpString: Expandable[String] = ExpandableString
}

object SubStringMatch {
	/**
		* Method to determine if the candidate String can become a super-string of goal by the addition of up to max more characters.
		*
		* @param max       the maximum number of additional characters that can be added to the candidate.
		* @param candidate the current value of candidate.
		* @param goal      the goal string which should end a completed candidate string.
		* @return the number of (appropriate) characters required to complete the candidate.
		*/
	def substringPrefix(max: Int, candidate: String, goal: String): Int = {
		def inner(w: String, r: Int): Int =
			if (w.isEmpty || (candidate endsWith w)) r
			else inner(w.substring(0, w.length - 1), r + 1)

		inner(goal, 0)
	}
}