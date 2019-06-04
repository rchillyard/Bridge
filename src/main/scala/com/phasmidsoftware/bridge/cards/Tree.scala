/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.{FitNode, Node, NodeException, Successors}
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

/**
	* This class represents a tree of State nodes.
	* Each State represents a sequence of card plays and current score of NS vs EW tricks.
	*
	* @param root the root node of the tree.
	*/
case class Tree(root: StateNode) extends Outputable[Unit] {

	/**
		* Expand the states of this Tree.
		*
		* @param levels the number of levels to enumerate.
		* @return a StateNode.
		*/
	def expand(levels: Int = Deal.CardsPerDeal)(success: State => Boolean, failure: State => Boolean = _ => false): StateNode = {
		trait SuccessorsState extends Successors[State] {
			def successors(state: State): Option[Seq[State]] = if (success(state)) None else if (failure(state)) Some(Nil) else Some(state.enumeratePlays)
		}
		implicit object SuccessorsState extends SuccessorsState

		root.expand(levels) match {
			case Some(n) => n.asInstanceOf[StateNode]
			case None => throw NodeException(s"unable to enumerate $levels plays for tree headed by ${root.state}")
		}
	}

	/**
		* Choose the plays for this Deal, by running expand for 52 levels, and terminating when NS have nsTricks or when EW have more than 13-nsTricks.
		*
		* @return a StateNode.
		*/
	def enumerateNoTrumpPlaysNS(nsTricks: Int): StateNode = expand()(State.goalFunction(directionNS = true, nsTricks))

	/**
		* Choose the plays for this Deal, by running expand for 52 levels, and terminating when NS have nsTricks or when EW have more than 13-nsTricks.
		*
		* @return a StateNode.
		*/
	def enumerateNoTrumpPlaysEW(ewTricks: Int): StateNode = expand()(State.goalFunction(directionNS = false, ewTricks))

	/**
		* Output this Tree to the given Output.
		*
		* @param output the output to append to.
		* @param xo     an optional value of X, defaulting to None.
		* @return a new instance of Output.
		*/
	def output(output: Output, xo: Option[Unit] = None): Output = root.output(output)
}

object Tree {
	/**
		* Method to create a Tree from a State.
		*
		* @param state the given State.
		* @return a new Tree based on the state as its root.
		*/
	def apply(state: State): Tree = apply(StateNode(state, done = false, Nil))

	/**
		* Method to create a Tree from a given Whist.
		*
		* @param whist the game.
		* @return a new Tree based on the given game.
		*/
	def apply(whist: Whist): Tree = apply(State(whist))

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



