package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.bridge.mcts.{FitNode, Node}
import com.phasmidsoftware.output.{Output, Outputable}

import scala.language.postfixOps

case class TrickNode(trick: Trick, followers: Seq[TrickNode]) extends FitNode[Trick](trick, followers) {
	/**
		* Method to form a Node from a Trick and from children.
		*
		* @param t   the given value of Trick.
		* @param tns the nodes which will be the children of the result.
		* @return a new Node based on t and tns.
		*/
	def unit(t: Trick, tns: Seq[Node[Trick]]): TrickNode = TrickNode(t, tns.asInstanceOf[Seq[TrickNode]])

	/**
		* Method to form a Node from a Trick.
		*
		* @param t the given value of Trick.
		* @return a new Node based on t, but with no children.
		*/
	override def unit(t: Trick): TrickNode = super.unit(t).asInstanceOf[TrickNode]

	/**
		* Method to add the given trick nodes to the children of this Node.
		*
		* @param tns the trick nodes to add as additional children.
		* @return a copy of this Node but with tns as additional children.
		*/
	override def ++(tns: Seq[Node[Trick]]): TrickNode = super.++(tns).asInstanceOf[TrickNode]

	/**
		* Method to add the given values to the children of this Node.
		*
		* @param ts the values to add as additional child values.
		* @return a copy of this Node but with ts as additional child values.
		*/
	override def :+(ts: Seq[Trick]): Node[Trick] = super.:+(ts).asInstanceOf[TrickNode]

	/**
		* Method to add the given node to the children of this Node.
		*
		* @param node the node to add as a child.
		* @return a copy of this Node but with node as an additional child.
		*/
	override def :+(node: Node[Trick]): Node[Trick] = super.:+(node).asInstanceOf[TrickNode]

	/**
		* Method to add the given x-value to the children of this Node.
		*
		* @param x the x value to be turned into a Node which is then :+'d to this Node.
		* @return a copy of this Node but with x as an additional child value.
		*/
	override def :+(x: Trick): Node[Trick] = super.:+(x).asInstanceOf[TrickNode]

	/**
		* Method to replace a node of this tree with the given node and to return the resulting tree.
		*
		* @param x the node to be replace.
		* @param y the node with which to replace the given node.
		* @return a copy of this TrickNode, but with node replaced by replacement.
		*/
	override def replace(x: Node[Trick], y: Node[Trick]): TrickNode = super.replace(x, y).asInstanceOf[TrickNode]

	/**
		* Method to append a node to the given node of this tree and to return the resulting tree.
		*
		* @param node     the node to which we append the appendee.
		* @param appendee the node to be appended to the given node.
		* @return a copy of this TrickNode, but with appendee appended to node
		*/
	override def append(node: Node[Trick], appendee: Node[Trick]): TrickNode = super.append(node, appendee).asInstanceOf[TrickNode]

	/**
		* Method to append a trick to the given node of this tree and to return the resulting tree.
		*
		* @param node  the node to which we append the appendee.
		* @param trick the trick to be appended to the given node.
		* @return a copy of this TrickNode, but with appendee appended to node
		*/
	override def append(node: Node[Trick], trick: Trick): TrickNode = super.append(node, trick).asInstanceOf[TrickNode]
}

case class Tree(deal: Deal, root: TrickNode) extends Outputable {

	def chooseLead(leader: Int): Seq[CardPlay] = deal.hands(leader).longestSuit.choosePlays(leader, FourthBest)

	/**
		* Choose the plays for this Deal, based on the prior plays.
		*
		* TODO make this tail-recursive.
		*
		* @param node   the node for which we wish to enumerate alternative plays.
		* @param levels is the limit of the number of cards for which to enumerate the plays (ideally should be 52).
		* @return a TrickNode.
		*/
	def enumeratePlays(node: TrickNode, levels: Int): TrickNode = if (levels > 0) {
		val trick = node.t
		val alternativeTricks: Seq[Trick] = trick.winner match {
			case Some(winner) => enumerateLeads(winner)
			case None => enumerateFollows(trick)
		}
		val nodeWithAltTricks = node :+ alternativeTricks
		println(nodeWithAltTricks)
		// TODO figure this out
		val result = nodeWithAltTricks.children.foldLeft(nodeWithAltTricks)((r, n) => r.append(n, enumeratePlays(n.asInstanceOf[TrickNode], levels - 1))).asInstanceOf[TrickNode]
		println(result)
		result
	}
	else node

	private def enumerateLeads(winner: Int): Seq[Trick] =
		for (p <- chooseLead(winner)) yield Trick(Seq(p), winner, p.suit)

	private def enumerateFollows(trick: Trick): Seq[Trick] =
		for (p <- deal.hands(trick.next).choosePlays(trick)) yield Trick(Seq(p), trick.leader, p.suit)

	def output(output: Output): Output = root.output(output)
}

object Tree {
	def apply(deal: Deal): Tree = apply(deal, TrickNode(Trick(Nil, 0, Spades), Nil))
}