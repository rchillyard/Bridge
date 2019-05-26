package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.{FitNode, Node, NodeException, Successors}
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

case class Tree(root: TreeNode) extends Outputable[Unit] {

	/**
		* Choose the plays for this Deal, based on the prior plays.
		*
		* @param levels the number of levels to enumerate.
		* @return a TreeNode.
		*/
	def enumeratePlays(levels: Int = 52)(success: State => Boolean, failure: State => Boolean): TreeNode = {
		trait SuccessorsState extends Successors[State] {
			def successors(t: State): Option[Seq[State]] = if (success(t)) None else if (failure(t)) Some(Nil) else Some(t.enumeratePlays)
		}
		implicit object SuccessorsState extends SuccessorsState

		root.expand(levels) match {
			case Some(n) => n.asInstanceOf[TreeNode]
			case None => throw NodeException(s"unable to enumerate $levels plays for tree headed by ${root.state}")
		}
	}

	def output(output: Output, xo: Option[Unit] = None): Output = root.output(output)
}

object Tree {
	def apply(state: State): Tree = apply(TreeNode(state, done = false, Nil))

	def apply(deal: Deal): Tree = apply(State(deal, Trick(0, Nil, 0, Spades), Tricks.zero))

	def makeStates(d: Deal, tricks: Tricks, ts: Seq[Trick]): Seq[State] = ts.map(t => State.create(d, t, tricks)).filter(_.fitness > 6)
}

/**
	* This represents a Node in the deal analysis tree.
	*
	* TODO rename as StateNode
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
	override def makeTerminal: TreeNode = super.makeTerminal.asInstanceOf[TreeNode]

	/**
		* Method to form a Node from a State and from children.
		*
		* @param t        the given value of State.
		* @param terminal true if the new node is to be marked terminal.
		* @param tns      the nodes which will be the children of the result.
		* @return a new Node based on t and tns.
		*/
	def unit(t: State, terminal: Boolean, tns: Seq[Node[State]]): TreeNode = TreeNode(t, done = terminal, tns.asInstanceOf[Seq[TreeNode]])

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



