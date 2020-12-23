/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.decisiontree.tree.LazyTree

import scala.language.postfixOps

/**
  * This class represents a tree of State nodes.
  * Each State represents a sequence of card plays and current score of NS vs EW tricks.
  *
  * @param state the state of the root of the tree.
  */
case class RevisedStateTree(state: State) extends LazyTree[State](state)(RevisedStateTree.nextMoves) {

  //  /**
  //    * Expand the states of this Tree.
  //    *
  //    * @param levels the number of levels to enumerate.
  //    * @return a StateNode.
  //    */
  //  override def expand(levels: Int = Deal.CardsPerDeal): StateNode[State] = super.expand(levels)

  //  /**
  //    * Choose the plays for this Deal, by running expand for 52 levels, and terminating when NS have nsTricks or when EW have more than 13-nsTricks.
  //    *
  //    * TODO this makes no sense
  //    *
  //    * @return a StateNode.
  //    */
  //  def enumerateNoTrumpPlaysNS(nsTricks: Int): StateNode[State] = expand()
}

object RevisedStateTree {
  def nextMoves: State => Seq[State] = state => state.enumeratePlays
}


