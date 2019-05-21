package com.phasmidsoftware.bridge.tree

import com.phasmidsoftware.output.{Output, Outputable}

trait Node[T] extends Outputable[Unit] {
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
		* TODO: watch out for the ordering of the children changing.
		*
		* @param x the node to be replace.
		* @param y the node with which to replace the given node.
		* @return a copy of this Node, but with node replaced by replacement.
		*/
	def replace(x: Node[T], y: Node[T]): Node[T] =
		if (children contains x) unit(t, children.filterNot(_ == x) :+ y)
		else unit(t, children map (n => n.replace(x, y)))

	/**
		* Method to replace a node of this tree optionally with the given node and to return the resulting tree.
		*
		* @param x   the node to be replace.
		* @param tno the optional node with which to replace the given node.
		* @return a copy of this Node, but with node replaced by replacement, assuming that tno exists; otherwise just return this.
		*/
	def replace(x: Node[T], tno: Option[Node[T]]): Node[T] =
		tno match {
			case Some(tn) => replace(x, tn)
			case None => this
		}

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
	def append(node: Node[T], appendees: Seq[Node[T]]): Node[T] = if (appendees.nonEmpty) replace(node, node ++ appendees) else this


	//	def dfs[Z](z: Z)(g: (Z, T) => Z): Unit = {
	//		val p = g(z, x)
	//		children.foreach(_.dfs(p)(g))
	//	}

	/**
		* Method to output this object (and, recursively, all of its children).
		*
		* @param output the output to append to.
		* @param xo     an optional value of X, defaulting to None.
		* @return a new instance of Output.
		*/
	def output(output: Output, xo: Option[Unit] = None): Output = outputChildren(outputValue(output))

	/**
		* Method to output the children of this Node (if any).
		* This default method will, if there are children, indent the output and insert a break, then recursively invoke output on the children.
		*
		* @param output the Output to append to.
		* @return a new instance of Output.
		*/
	def outputChildren(output: Output): Output = if (children.nonEmpty) {
		val indentedOutput = output.indent("  ")
		indentedOutput ++ children.map(_.output(indentedOutput.copy.insertBreak))
	} else output

	/**
		* Method to output the value of this Node.
		* This default method will use Output if the value is itself Outputable, otherwise it will use toString.
		*
		* @param output the Output to append to.
		* @return a new instance of Output.
		*/
	def outputValue(output: Output): Output = t match {
		case o: Outputable[_] => o.output(output)
		case _ => output :+ t
	}
}

case class NodeException(str: String) extends Exception(str)
