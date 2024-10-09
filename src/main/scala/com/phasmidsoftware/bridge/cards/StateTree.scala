/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.phasmidsoftware.decisiontree.{Expandable, GoalDriven, StateNode, Tree}
import com.phasmidsoftware.util.Show

import scala.language.postfixOps

/**
  * This class represents a tree of State nodes.
  * Each State represents a sequence of card plays and current score of NS vs EW tricks.
  *
  * @param root the root node of the tree.
  */
case class StateTree(override val root: StateNode[State]) extends Tree[State](root) {

  /**
    * Expand the states of this Tree.
    *
    * @param levels the number of levels to enumerate.
    * @return a StateNode.
    */
  override def expand(levels: Int = Deal.CardsPerDeal): StateNode[State] = super.expand(levels)

  /**
    * Choose the plays for this Deal, by running expand for 52 levels, and terminating when NS have nsTricks or when EW have more than 13-nsTricks.
    *
    * CONSIDER this makes no sense
    *
    * @return a StateNode.
    */
  def enumerateNoTrumpPlaysNS(nsTricks: Int): StateNode[State] = expand()
}

object StateTree {
  /**
    * Method to create a Tree from a given Whist.
    *
    * @param whist the game.
    * @return a new Tree based on the given game.
    */
  def apply(whist: Whist)(implicit ev1: Expandable[State], ev2: GoalDriven[State], ev3: Show[State]): StateTree = {
    State.count = 0
    StateTree(StateNode(State(whist), None, Nil))
  }
}
