/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.tree

import com.phasmidsoftware.output.{Output, Outputable}

/**
	* This trait defines the behavior of a node in a tree.
	*
	* @tparam T the underlying type of the node.
	*/
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
		* Method to mark a terminal Node.
		* Once such a node is encountered, all further tree building must cease.
		*
		* @return true if this Node is terminal.
		*/
	def isTerminal: Boolean

	/**
		* Method to expand a branch of a tree, by taking this Node and replacing it with children nodes which are themselves recursively expanded.
		* The algorithm operates in a depth-first-search manner.
		*
		* If successor(t) yields a Some(Nil), it simply means that this branch will be eliminated, while other branches will continue as normal.
		* If successor(t) yields None, we break out of the expansion and return this node with the successful node marked terminal.
		* Note, however, some siblings, uncles and aunts of the success node will remain in this.
		*
		* TODO make this tail-recursive
		*
		* @tparam U a super-type of T (or T, of course) for which there is evidence of Successors[U].
		* @return an Option of Node[U]
		*/
	def expand[U >: T : Successors](levels: Int): Option[Node[U]] =
		if (levels <= 0) None
		else {
			def replaceExpandedChild(r: Node[U], n: Node[U]) = if (r.isTerminal) r else r.replace(n, n.expand(levels - 1))

			val node = this.asInstanceOf[Node[U]]
			implicitly[Successors[U]].successors(t) match {
				case Some(Nil) => // no descendants? signal for this Node to be removed.
					None
				case None => // terminating condition found? mark it.
					Some(node.makeTerminal)
				case Some(us) => // normal situation with descendants? recursively expand them.
					val z = node :+ us
					Some(z.children.foldLeft(z)(replaceExpandedChild))
			}
		}

	/**
		* Method to form a Node from a T.
		*
		* @param t   the given value of T.
		* @param tns the nodes which will be the children of the result.
		* @return a new Node based on t and tns.
		*/
	def unit(t: T, terminal: Boolean, tns: Seq[Node[T]]): Node[T]

	/**
		* Method to form a Node from a T.
		*
		* @param t the given value of T.
		* @return a new Node based on t, but with no children.
		*/
	def unit(t: T): Node[T] = unit(t, terminal = false, Nil)

	/**
		* Method to mark this Node as terminal (i.e. success).
		*
		* @return a new copy of this Node but with isTerminal set to true.
		*/
	def makeTerminal: Node[T] = unit(t, terminal = true, children)

	/**
		* Method to add the given tns to the children of this Node.
		*
		* @param tns the tns to add as additional children.
		* @return a copy of this Node but with tns as additional children.
		*/
	def ++(tns: Seq[Node[T]]): Node[T] = unit(t, terminal = isTerminal, children ++ tns)

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
	def :+(node: Node[T]): Node[T] = unit(t, terminal = isTerminal, children :+ node)

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
		* CONSIDER: whether isTerminal should be y.isTerminal in the else part (I don't think so).
		*
		* @param x the node to be replaced.
		* @param y the node with which to replace the given node.
		* @return a copy of this Node, but with node replaced by replacement.
		*/
	def replace(x: Node[T], y: Node[T]): Node[T] =
		if (x == y) this
		else if (children contains x) unit(t, y.isTerminal, children.map(n => if (n eq x) y else n))
		else unit(t, y.isTerminal, children map (n => n.replace(x, y)))

	/**
		* Method to remove the child x from this Node
		*
		* @param x the node to be removed.
		* @return a copy of this Node, but with node removed.
		*/
	def remove(x: Node[T]): Node[T] = if (children contains x) unit(t, isTerminal, children.flatMap(n => if (n eq x) None else Some(n)))
	else unit(t, isTerminal, children map (n => n.remove(x)))

	/**
		* Method to replace a node of this tree optionally with the given node and to return the resulting tree.
		*
		* @param x   the node to be replaced.
		* @param tno the optional node with which to replace the given node.
		* @return a copy of this Node, but with node replaced by replacement, assuming that tno exists; otherwise just return this.
		*/
	def replace(x: Node[T], tno: Option[Node[T]]): Node[T] =
		tno match {
			case Some(tn) => replace(x, tn)
			// NOTE: we remove a parent who produces no children. The idea is to reduce strain on the GC.
			case None => remove(x)
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

	/**
		* Method to perform depth-first-search on this Node.
		*
		* @param z an initial (zero) value of Z.
		* @param g a function to combine a Z and a T into a Z.
		* @tparam Z the underlying type of
		*/
	def dfs[Z](z: Z)(g: (Z, T) => Z): Z = children.foldLeft(g(z, t))((r, tn) => tn.dfs(r)(g))

	/**
		* Method to traverse this Node, returning a list of T values in depth-first order.
		*/
	lazy val depthFirstTraverse: List[T] = dfs(List[T]())((z, t) => t +: z)

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

trait Successors[T] {
	/**
		* Method to yield the successors (i.e. children) of the underlying type T.
		*
		* @param t the value of T.
		* @return an Option of Seq[T].
		*         If the return is Some(Seq(...)) then the content of the option is the list of successors.
		*         If the result is Some(Nil), it signifies that the given value of t holds no promise and therefore should not be further expanded.
		*         Otherwise, if the return is None, it signifies that we are to stop building our tree and immediately return the existing tree.
		*/
	def successors(t: T): Option[Seq[T]]
}
