package com.phasmidsoftware.bridge.mcts

import com.phasmidsoftware.output.{Output, Outputable}

trait Node[T] extends Outputable {
	/**
		* Method to yield the value of this Node.
		*
		* @return a value of T
		*/
	def t: T

	/**
		* Method to yield the children of this Node.
		*
		* @return a sequence of Node[T]
		*/
	def children: Seq[Node[T]]

	/**
		* Method to form a Node from a T.
		*
		* @param t   the given value of T.
		* @param tns the nodes which will be the children of the result.
		* @return a new Node based on t and tns.
		*/
	def unit(t: T, tns: Seq[Node[T]]): Node[T]

	/**
		* Method to form a Node from a T.
		*
		* @param t the given value of T.
		* @return a new Node based on t, but with no children.
		*/
	def unit(t: T): Node[T] = unit(t, Nil)

	/**
		* Method to add the given tns to the children of this Node.
		*
		* @param tns the tns to add as additional children.
		* @return a copy of this Node but with tns as additional children.
		*/
	def ++(tns: Seq[Node[T]]): Node[T] = unit(t, children ++ tns)

	/**
		* Method to add the given values to the children of this Node.
		*
		* @param ts the values to add as additional child values.
		* @return a copy of this Node but with ts as additional child values.
		*/
	def :+(ts: Seq[T]): Node[T] = this ++ (ts map unit)

	/**
		* Method to add the given node to the children of this Node.
		*
		* CONSIDER: adding the node at the head of the list of children.
		*
		* @param node the node to add as a child.
		* @return a copy of this Node but with node as an additional child.
		*/
	def :+(node: Node[T]): Node[T] = unit(t, children :+ node)

	/**
		* Method to add the given x-value to the children of this Node.
		*
		* @param x the x value to be turned into a Node which is then :+'d to this Node.
		* @return a copy of this Node but with x as an additional child value.
		*/
	def :+(x: T): Node[T] = this :+ unit(x)

	/**
		* Method to replace a node of this tree with the given node and to return the resulting tree.
		*
		* @param x the node to be replace.
		* @param y the node with which to replace the given node.
		* @return a copy of this Node, but with node replaced by replacement.
		*/
	def replace(x: Node[T], y: Node[T]): Node[T] =
		if (children contains x) unit(t, children.filterNot(_ == x) :+ y)
		else unit(t, children map (n => n.replace(x, y)))

	/**
		* Method to append a node to the given node of this tree and to return the resulting tree.
		*
		* @param node     the node to which we append the appendee.
		* @param appendee the node to be appended to the given node.
		* @return a copy of this Node but with appendee appended to node.
		*/
	def append(node: Node[T], appendee: Node[T]): Node[T] = replace(node, node :+ appendee)

	/**
		* Method to append a value to the given node of this tree and to return the resulting tree.
		*
		* @param node the node to which we append the appendee.
		* @param t    the value to be appended to the given node.
		* @return a copy of this Node but with t appended to node.
		*/
	def append(node: Node[T], t: T): Node[T] = append(node, unit(t))

	/**
		* Method to append a node to the given node of this tree and to return the resulting tree.
		*
		* @param node      the node to which we append the appendee.
		* @param appendees the node to be appended to the given node.
		* @return a copy of this Node but with appendee appended to node.
		*/
	def append(node: Node[T], appendees: Seq[Node[T]]): Node[T] = replace(node, node ++ appendees)


	//	def dfs[Z](z: Z)(g: (Z, T) => Z): Unit = {
	//		val p = g(z, x)
	//		children.foreach(_.dfs(p)(g))
	//	}

	/**
		* Method to output this Node (and, recursively, all of its children).
		*
		* @param output the output to append to.
		* @return a new instance of Output.
		*/
	def output(output: Output): Output = outputChildren(outputValue(output))

	/**
		* Method to output the children of this Node (if any).
		* This default method will, if there are children, indent the output and insert a break, then recursively invoke ouput on the children.
		*
		* @param output the Output to append to.
		* @return a new instance of Output.
		*/
	def outputChildren(output: Output): Output = if (children.nonEmpty) {
		val indentedOutput = output.indent("  ").insertBreak
		indentedOutput ++ children.map(_.output(indentedOutput.copy))
	} else output

	/**
		* Method to output the value of this Node.
		* This default method will use Output if the value is itself Outputable, otherwise it will use toString.
		*
		* @param output the Output to append to.
		* @return a new instance of Output.
		*/
	def outputValue(output: Output): Output = t match {
		case o: Outputable => o.output(output)
		case _ => output :+ t
	}
}

//object FP {
//	/**
//		* TODO unit test and replace in LaScala source.
//		*
//		* @param xos a sequence of Option[X] values
//		* @tparam X the underlying type
//		* @return a sequence of X values wrapped in Option
//		*         NOTE: that the output collection type will be Seq, regardless of the input type
//		*/
//	def sequence[X](xos: Seq[Option[X]]): Option[Seq[X]] = (Option(Seq[X]()) /: xos) {
//		(xso, xo) =>
//			for (xs <- xso) yield xo match {
//				case Some(x) => xs :+ x
//				case None => xs
//			}
//	}
//}

object Node {
	//	/**
	//		* Method to take a Seq of Option of T and yield an Option of T.
	//		*
	//		* @param tos a Seq of Option of T.
	//		* @tparam T the underlying type.
	//		* @return an Option[T]
	//		* @throws NodeException if there is more than one Some(t) in the input.
	//		*/
	//	def sequence[T](tos: Seq[Option[T]]): Option[T] = FP.sequence(tos) match {
	//		case None => None
	//		case Some(ns) => ns match {
	//			case Nil => None
	//				case n :: Nil => Some(n)
	//				case _ => throw NodeException(s"Node.sequence: logic error")
	//		}
	//	}

}

case class NodeException(str: String) extends Exception(str)
