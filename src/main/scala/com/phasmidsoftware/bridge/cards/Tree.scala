package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.{FitNode, Node, NodeException, Successors}
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

case class Tree(root: StateNode) extends Outputable[Unit] {

	/**
		* Choose the plays for this Deal, based on the prior plays.
		*
		* @param levels the number of levels to enumerate.
		* @return a StateNode.
		*/
	def enumeratePlays(levels: Int = 52)(success: State => Boolean, failure: State => Boolean): StateNode = {
		trait SuccessorsState extends Successors[State] {
			def successors(t: State): Option[Seq[State]] = if (success(t)) None else if (failure(t)) Some(Nil) else Some(t.enumeratePlays)
		}
		implicit object SuccessorsState extends SuccessorsState

		root.expand(levels) match {
			case Some(n) => n.asInstanceOf[StateNode]
			case None => throw NodeException(s"unable to enumerate $levels plays for tree headed by ${root.state}")
		}
	}

	/**
		* Choose the plays for this Deal, by running enumeratePlays for 52 levels, and terminating when NS have nsTricks or when EW have more than 13-nsTricks.
		*
		* @return a StateNode.
		*/
	def enumerateNoTrumpPlays(nsTricks: Int): StateNode = enumeratePlays()(_.tricks.ns >= nsTricks, _.tricks.ew > 13 - nsTricks)

	def output(output: Output, xo: Option[Unit] = None): Output = root.output(output)
}

object Tree {
	def apply(state: State): Tree = apply(StateNode(state, done = false, Nil))

	def apply(deal: Deal): Tree = apply(State(deal, Trick(0, Nil, 0, Spades), Tricks.zero))

	def makeStates(d: Deal, tricks: Tricks, ts: Seq[Trick]): Seq[State] = ts.map(t => State.create(d, t, tricks)).filter(_.fitness > 6)
}

/**
	* This represents a node in the deal analysis tree.
	*
	* @param state     a Trick/Deal combination: the trick is in general incomplete: each node represents a different play.
	* @param done      true if this is a terminal Node (see definition of Node[X]).
	* @param followers the children of this node, i.e. the nodes which will follow.
	*/
case class StateNode(state: State, done: Boolean, followers: Seq[StateNode]) extends FitNode[State](state, done, followers) {

	/**
		* Make a new version of this Node which is terminal.
		*
		* @return a copy of this StateNode but terminal.
		*/
	override def makeTerminal: StateNode = super.makeTerminal.asInstanceOf[StateNode]

	/**
		* Method to form a Node from a State and from children.
		*
		* @param t        the given value of State.
		* @param terminal true if the new node is to be marked terminal.
		* @param tns      the nodes which will be the children of the result.
		* @return a new Node based on t and tns.
		*/
	def unit(t: State, terminal: Boolean, tns: Seq[Node[State]]): StateNode = StateNode(t, done = terminal, tns.asInstanceOf[Seq[StateNode]])

	/**
		* Method to form a Node from a State.
		*
		* @param t the given value of State.
		* @return a new Node based on t, but with no children.
		*/
	override def unit(t: State): StateNode = super.unit(t).asInstanceOf[StateNode]

	/**
		* Method to add the given tree nodes to the children of this Node.
		*
		* @param tns the trick nodes to add as additional children.
		* @return a copy of this Node but with tns as additional children.
		*/
	override def ++(tns: Seq[Node[State]]): StateNode = super.++(tns).asInstanceOf[StateNode]

	/**
		* Method to add the given values to the children of this Node.
		*
		* @param ts the values to add as additional child values.
		* @return a copy of this Node but with ts as additional child values.
		*/
	override def :+(ts: Seq[State]): Node[State] = super.:+(ts).asInstanceOf[StateNode]

	/**
		* Method to add the given node to the children of this Node.
		*
		* @param node the node to add as a child.
		* @return a copy of this Node but with node as an additional child.
		*/
	override def :+(node: Node[State]): Node[State] = super.:+(node).asInstanceOf[StateNode]

	/**
		* Method to add the given x-value to the children of this Node.
		*
		* @param x the x value to be turned into a Node which is then :+'d to this Node.
		* @return a copy of this Node but with x as an additional child value.
		*/
	override def :+(x: State): Node[State] = super.:+(x).asInstanceOf[StateNode]

	/**
		* Method to replace a node of this tree with the given node and to return the resulting tree.
		*
		* @param x the node to be replace.
		* @param y the node with which to replace the given node.
		* @return a copy of this StateNode, but with node replaced by replacement.
		*/
	override def replace(x: Node[State], y: Node[State]): StateNode = super.replace(x, y).asInstanceOf[StateNode]

	/**
		* Method to append a node to the given node of this tree and to return the resulting tree.
		*
		* @param node     the node to which we append the appendee.
		* @param appendee the node to be appended to the given node.
		* @return a copy of this StateNode, but with appendee appended to node
		*/
	override def append(node: Node[State], appendee: Node[State]): StateNode = super.append(node, appendee).asInstanceOf[StateNode]

	/**
		* Method to append a state to the given node of this tree and to return the resulting tree.
		*
		* @param node  the node to which we append the appendee.
		* @param state the state to be appended to the given node.
		* @return a copy of this StateNode, but with appendee appended to node
		*/
	override def append(node: Node[State], state: State): StateNode = super.append(node, state).asInstanceOf[StateNode]
}



