package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.tree.{FitNode, Fitness, Node, NodeException}
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

/**
	* Case class State to represent a possible state of play in analysis of a Deal.
	* The state must be consistent, which is to say that the deal is the result of playing all of the previous tricks AND
	* the current (possibly partial) trick.
	*
	* CONSIDER eliminating trick from this State and then we can simply replace by Deal.
	*
	* @param deal  the Deal.
	* @param trick the Trick.
	*/
case class State(deal: Deal, trick: Trick) extends Outputable[Unit] {

	/**
		* Initialization: just checking that this State is consistent.
		*
		* TODO remove this check.
		*/
	if (!isConsistent) System.err.println(s"state not consistent ${deal.cards}, $deal: $trick")

	/**
		* Method to get the next State in sequence.
		*
		* @param t the next Trick.
		* @return a new consistent State based on deal and t.
		*/
	def next(t: Trick): State = State.create(deal, t)

	/**
		* @return true if the number of cards played according to the trick plus the number of cares remaining in the deal equals 52
		*/
	def isConsistent: Boolean = cardsPlayed + deal.cards == 52 // && validate

	/**
		* Method to validate this State.
		*
		* @return true if all the plays of the trick are validated
		*/
	def validate: Boolean = trick.plays.map(_.validate).forall(_ == true)

	/**
		* The total number of cards played according to
		*
		* @return
		*/
	def cardsPlayed: Int = trick.index * 4 + trick.size

	override def toString: String = s"${deal.neatOutput} $trick"

	def output(output: Output, xo: Option[Unit] = None): Output = trick.output(output, Some(deal))
}

object State {

	/**
		* Method to create an initial state based on a deal.
		*
		* TODO we need a better representation of a non-trick.
		*
		* @param deal the Deal of the new State.
		* @return a new State based on the Deal without any tricks having been played.
		*/
	def apply(deal: Deal): State = apply(deal, Trick(0, Nil, 0, Spades))

	def create(deal: Deal, trick: Trick): State = {
		if (trick.isComplete) State(deal.play(trick.plays.last).quit, trick)
		else State(deal.play(trick.plays.last), trick)
	}

	implicit object StateFitness extends Fitness[State] {
		override def fitness(x: State): Double = x.trick.evaluate
	}
}

case class Tree(root: TreeNode) extends Outputable[Unit] {

	/**
		* Choose the plays for this Deal, based on the prior plays.
		*
		* TODO make this tail-recursive.
		*
		* @param levels the number of levels to enumerate.
		* @return a TreeNode.
		*/
	def enumeratePlays(levels: Int = 52): TreeNode = Tree.enumeratePlays(root, levels) match {
		case Some(n) => n
		case None => throw NodeException(s"unable to enumerate 52 plays for tree headed by ${root.state}")
	}

	def output(output: Output, xo: Option[Unit] = None): Output = root.output(output)
}

object Tree {
	def apply(deal: Deal): Tree = apply(TreeNode(State(deal, Trick(0, Nil, 0, Spades)), Nil))

	def makeStates(deal: Deal, tricks: Seq[Trick]): Seq[State] = tricks map (t => State.create(deal, t))

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
	def enumeratePlays(node: TreeNode, levels: Int): Option[TreeNode] = if (levels > 0) {
		val state = node.t
		val trick = state.trick
		val altStates = trick.winner match {
			case Some(winner) => enumerateLeads(state.deal, trick.index + 1, winner)
			case None => enumerateFollows(state)
		}
		val nodeWithAltStates: Node[State] = node :+ altStates
		Some(nodeWithAltStates.children.foldLeft(nodeWithAltStates)((r, n) => r.replace(n, enumeratePlays(n.asInstanceOf[TreeNode], levels - 1))).asInstanceOf[TreeNode])
	}
	else None

	//	private
	def enumerateLeads(deal: Deal, trickIndex: Int, leader: Int): Seq[State] =
		Tree.makeStates(deal, for (p <- chooseLead(deal, leader)) yield Trick(trickIndex, Seq(p), leader, p.suit))

	//	private
	def enumerateFollows(state: State): Seq[State] =
		Tree.makeStates(state.deal, for (p <- state.deal.hands(state.trick.next).choosePlays(state.trick)) yield state.trick :+ p)

	//	private
	def chooseLead(deal: Deal, leader: Int): Seq[CardPlay] = deal.hands(leader).longestSuit.choosePlays(deal, leader, FourthBest)

}

/**
	* This represents a Node in the deal analysis tree.
	*
	* @param state     a Trick/Deal combination: the trick is in general incomplete: each node represents a different play.
	* @param followers the children of this node, i.e. the nodes which will follow.
	*/
case class TreeNode(state: State, followers: Seq[TreeNode]) extends FitNode[State](state, followers) {

	/**
		* Method to form a Node from a State and from children.
		*
		* @param t   the given value of State.
		* @param tns the nodes which will be the children of the result.
		* @return a new Node based on t and tns.
		*/
	def unit(t: State, tns: Seq[Node[State]]): TreeNode = TreeNode(t, tns.asInstanceOf[Seq[TreeNode]])

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



