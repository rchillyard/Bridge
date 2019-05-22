package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.{FitNode, Node, NodeException}
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

case class Tree(root: TreeNode) extends Outputable[Unit] {

	/**
		* Choose the plays for this Deal, based on the prior plays.
		*
		* TODO make this tail-recursive.
		*
		* @param levels the number of levels to enumerate.
		* @return a TreeNode.
		*/
	def enumeratePlays(levels: Int = 52)(success: State => Boolean, failure: State => Boolean): TreeNode = Tree.enumeratePlays(root, levels)(success, failure) match {
		case Some(n) => n
		case None => throw NodeException(s"unable to enumerate 52 plays for tree headed by ${root.state}")
	}

	def output(output: Output, xo: Option[Unit] = None): Output = root.output(output)
}

object Tree {
	def apply(state: State): Tree = apply(TreeNode(state, done = false, Nil))

	def apply(deal: Deal): Tree = apply(State(deal, Trick(0, Nil, 0, Spades), Tricks.zero))

	def makeStates(d: Deal, tricks: Tricks, ts: Seq[Trick]): Seq[State] = ts.map(t => State.create(d, t, tricks)).filter(_.fitness > 6)
	/**
		* Choose the plays for this Deal, based on the prior plays.
		*
		* TODO make this tail-recursive.
		*
		* TODO generalize this and move it into Node, and/or FitNode.
		*
		* @param node   the node for which we wish to enumerate alternative plays.
		* @param levels is the limit of the number of cards for which to enumerate the plays (ideally should be 52).
		* @return a TreeNode.
		*/
	def enumeratePlays(node: TreeNode, levels: Int)(success: State => Boolean, failure: State => Boolean): Option[TreeNode] = if (levels > 0) {
		val state = node.t
		if (success(state))
			Some(node.makeTerminal.asInstanceOf[TreeNode])
		else if (failure(state))
			None
		else {
			val nodeWithAltStates: Node[State] = node :+ state.enumeratePlays
			Some(nodeWithAltStates.children.foldLeft(nodeWithAltStates)((r, n) => combineNodes(levels, success, failure, r, n)).asInstanceOf[TreeNode])
		}
	}
	else None

	private def combineNodes(levels: Int, success: State => Boolean, failure: State => Boolean, r: Node[State], n: Node[State]) =
		if (n.terminal) r.makeTerminal else r.replace(n, enumeratePlays(n.asInstanceOf[TreeNode], levels - 1)(success, failure))

	//	if (r.terminal) r else r.replace(n, enumeratePlays(n.asInstanceOf[TreeNode], levels - 1)(success, failure))
}

/**
	* This represents a Node in the deal analysis tree.
	*
	* @param state     a Trick/Deal combination: the trick is in general incomplete: each node represents a different play.
	* @param done      true if this is a terminal Node (see definition of Node[X]).
	* @param followers the children of this node, i.e. the nodes which will follow.
	*/
case class TreeNode(state: State, done: Boolean, followers: Seq[TreeNode]) extends FitNode[State](state, done, followers) {
	/**
		* Make a new version of this Node which is terminal.
		*
		* @return a copy of this TreeNode but terminal.
		*/
	//	def makeTerminal: TreeNode = TreeNode(state, done = true, followers)

	/**
		* Method to form a Node from a State and from children.
		*
		* @param t   the given value of State.
		* @param tns the nodes which will be the children of the result.
		* @return a new Node based on t and tns.
		*/
	def unit(t: State, terminal: Boolean, tns: Seq[Node[State]]): TreeNode = TreeNode(t, done = false, tns.asInstanceOf[Seq[TreeNode]])

	/**
		* Method to form a Node from a State.
		*
		* @param t the given value of State.
		* @return a new Node based on t, but with no children.
		*/
	override def unit(t: State): TreeNode = super.unit(t).asInstanceOf[TreeNode]

	/**
		* Method to add the given tree nodes to the children of this Node.
		*
		* @param tns the trick nodes to add as additional children.
		* @return a copy of this Node but with tns as additional children.
		*/
	override def ++(tns: Seq[Node[State]]): TreeNode = super.++(tns).asInstanceOf[TreeNode]

	/**
		* Method to add the given values to the children of this Node.
		*
		* @param ts the values to add as additional child values.
		* @return a copy of this Node but with ts as additional child values.
		*/
	override def :+(ts: Seq[State]): Node[State] = super.:+(ts).asInstanceOf[TreeNode]

	/**
		* Method to add the given node to the children of this Node.
		*
		* @param node the node to add as a child.
		* @return a copy of this Node but with node as an additional child.
		*/
	override def :+(node: Node[State]): Node[State] = super.:+(node).asInstanceOf[TreeNode]

	/**
		* Method to add the given x-value to the children of this Node.
		*
		* @param x the x value to be turned into a Node which is then :+'d to this Node.
		* @return a copy of this Node but with x as an additional child value.
		*/
	override def :+(x: State): Node[State] = super.:+(x).asInstanceOf[TreeNode]

	/**
		* Method to replace a node of this tree with the given node and to return the resulting tree.
		*
		* @param x the node to be replace.
		* @param y the node with which to replace the given node.
		* @return a copy of this TreeNode, but with node replaced by replacement.
		*/
	override def replace(x: Node[State], y: Node[State]): TreeNode = super.replace(x, y).asInstanceOf[TreeNode]

	/**
		* Method to append a node to the given node of this tree and to return the resulting tree.
		*
		* @param node     the node to which we append the appendee.
		* @param appendee the node to be appended to the given node.
		* @return a copy of this TreeNode, but with appendee appended to node
		*/
	override def append(node: Node[State], appendee: Node[State]): TreeNode = super.append(node, appendee).asInstanceOf[TreeNode]

	/**
		* Method to append a state to the given node of this tree and to return the resulting tree.
		*
		* @param node  the node to which we append the appendee.
		* @param state the state to be appended to the given node.
		* @return a copy of this TreeNode, but with appendee appended to node
		*/
	override def append(node: Node[State], state: State): TreeNode = super.append(node, state).asInstanceOf[TreeNode]
}



