/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.tree

import com.phasmidsoftware.output.{Loggable, Loggables}


/**
  * Abstract class representing a Node with a fitness.
  *
  * @param t        the value of this Node.
  * @param decided  indicator of the node having terminated the expansion of the tree.
  * @param children the children nodes of this Node.
  * @tparam T the underlying type of the nodes, for which there must be evidence of Fitness.
  */
abstract class ExpandingNode[T: Expandable : Loggable](val t: T, val decided: Option[Boolean], val children: Seq[ExpandingNode[T]]) extends Node[T] {

  /**
    * Method to form a Node from a T.
    *
    * @param _t  the given value of T.
    * @param tns the nodes which will be the children of the result.
    * @return a new Node based on t and tns.
    */
  def unit(_t: T, tns: Seq[Node[T]]): ExpandingNode[T] = unit(_t, None, tns)

  /**
    * Method to form a Node from a T.
    *
    * CONSIDER rename decided
    *
    * @param _t      the given value of T.
    * @param decided an optional Boolean
    * @param tns     the nodes which will be the children of the result.
    * @return a new Node based on t and tns.
    */
  def unit(_t: T, decided: Option[Boolean], tns: Seq[Node[T]]): ExpandingNode[T]

  /**
    * Method to expand a branch of a tree, by taking this Node and replacing it with children nodes which are themselves recursively expanded.
    * The algorithm operates in a depth-first-search manner.
    *
    * If successor(t) yields Some(Nil), it simply means that this branch will be eliminated, while other branches will continue as normal.
    * If successor(t) yields None, we break out of the expansion and return this node with the successful node marked decided.
    * Note, however, some siblings, uncles and aunts of the success node will remain in this.
    *
    * TODO make this tail-recursive
    *
    * @return an Option of Node[T]
    */
  def expand(levels: Int): Option[ExpandingNode[T]] =
    if (levels <= 0) None
    else Some {
      import com.phasmidsoftware.output.Flog._
      def replaceExpandedChild(r: ExpandingNode[T], n: ExpandingNode[T]): ExpandingNode[T] = r.decided match {
        //				case Some(goal) => if (implicitly[Successors[U]].canDecide(n.t, !goal)) r.replace(n, n.expand(levels - 1)) else r.remove()
        // TODO we need to pay attention to goal
        case Some(goal) => s"replaceExpandedChild: $t, $goal, r:" |! r.decide(goal)
        case None => s"replaceExpandedChild: $t, replacement:" |! r.replace(n, n.expand(levels - 1))
      }

      implicit val loggableExpandingNode: Loggable[ExpandingNode[T]] = ExpandingNode.expandingNodeLogger
      //      implicit val loggableOption: Loggable[Option[ExpandingNode[T]]] = ExpandingNode.optionLoggable[ExpandingNode[T]]

      // TODO we need to pass in the currently decided state instead of None:
      implicitly[Expandable[T]].result(t, None) match {
        case Right(Nil) => // XXX no descendants? signal for this Node to be removed.
          s"expand: ${implicitly[Loggable[T]].toLog(t)}, Right(Nil) result:" |! this // TODO clean up.
        case Right(ts) => // XXX normal situation with descendants? recursively expand them.
          val thisNodeWithSuccessors = this :+ ts
          s"expand: ${implicitly[Loggable[T]].toLog(t)}, Right(ts) result:" |! thisNodeWithSuccessors.children.foldLeft(thisNodeWithSuccessors)(replaceExpandedChild)
        case Left(b) => // XXX terminating condition found? mark it.
          s"expand: ${implicitly[Loggable[T]].toLog(t)}, " +
            s"left($b) result:" |! decide(b)
      }
    }

  /**
    * Method to replace a node of this tree optionally with the given node and to return the resulting tree.
    *
    * @param x   the node to be replaced.
    * @param tno the optional node with which to replace the given node.
    * @return a copy of this Node, but with node x replaced by
    *         (1) if tno exists, then its value;
    *         (2) if tno does not exist, then nothing.
    */
  def replace(x: ExpandingNode[T], tno: Option[ExpandingNode[T]]): ExpandingNode[T] =
    tno match {
      case Some(tn) => replace(x, tn)
      // NOTE: we remove a parent who produces no children. The idea is to reduce strain on the GC.
      case None => remove(x)
    }


  /**
    * Method to mark this Node as decided (i.e. success is true or false).
    *
    * @param success true if the goal has been reached, false if the contra-goal has been reached.
    * @return a new copy of this Node but with decided set to the Some(success).
    */
  def decide(success: Boolean): ExpandingNode[T] = unit(t, Some(success), children)

  override def replace(x: Node[T], y: Node[T]): ExpandingNode[T] = {
    val result = super.replace(x, y).asInstanceOf[ExpandingNode[T]]
    y.asInstanceOf[ExpandingNode[T]].decided match {
      case Some(b) => result.decide(b)
      case None => result
    }
  }

  //    super.replace(x, y).asInstanceOf[ExpandingNode[T]]

  override def remove(x: Node[T]): ExpandingNode[T] = super.remove(x).asInstanceOf[ExpandingNode[T]]

  override def unit(t: T): ExpandingNode[T] = super.unit(t).asInstanceOf[ExpandingNode[T]]

  /**
    * Method to add the given values to the children of this Node.
    *
    * @param ts the values to add as additional child values.
    * @return a copy of this Node but with ts as additional child values.
    */
  override def :+(ts: Seq[T]): ExpandingNode[T] = super.:+(ts).asInstanceOf[ExpandingNode[T]]
}

object ExpandingNode extends Loggables {

  def expandingNodeLogger[T: Loggable]: Loggable[ExpandingNode[T]] = (t: ExpandingNode[T]) => {

    val wT = implicitly[Loggable[T]].toLog(t.t)
    val wDecided = t.decided match {
      case Some(b) => s" (decided=$b)"
      case None => ""
    }
    val wFollowers = s" with ${t.children.size}"
    wT + wDecided + wFollowers
  }
}

/**
  * This is the trait (actually, it's the base of a type-class) which allows
  * the application to program the manner of expanding a tree.
  *
  * @tparam T the underlying type (matches the T of a Node).
  */
trait Expandable[T] {
  /**
    * Method to determine if a decision has been reached based on the given value of t.
    * In such a case, expansion should terminate (not necessarily immediately).
    *
    * @param t the value of t to consider.
    * @return if non-deciding, then None is returned.
    *         Otherwise Some(b) where b indicates a decision of success or failure.
    */
  def decide(t: T): Option[Boolean]

  /**
    * Method to determine if a decision can be reached based on the given value of t,
    * and the value of to.
    * If to is None, then the result is always true.
    * Otherwise, the T value represents the previously achieved goal.
    * If t could possibly be expanded to counter the previously achieved goal, then true is returned.
    * But, if it's impossible for t to counter said goal, then false is returned.
    *
    * @param t  the value of t to consider.
    * @param to the achieved goal state.
    * @return true if it's mathematically possible to yield a result.
    */
  def canDecide(t: T, to: Option[T]): Boolean

  /**
    * Method to yield the successors (i.e. children) of the underlying type T for purposes of node expansion.
    *
    * @param t the value of T.
    * @return a Seq[T] containing the successors (children) of T.
    */
  def successors(t: T): Seq[T]

  /**
    * Method to yield the result (i.e. children) of the underlying type T.
    *
    * Either a list of new child nodes is returned or we return a Boolean to signify the reaching of a goal.
    * The value of the Boolean signifies whether it was a positive goal or a negative goal.
    * For example, when the T values represent states of play in Bridge,
    * a positive goal is that the protagonist (declaring side) wins a specified number of tricks, say nine.
    * A negative goal would be when the antagonist (defending side) wins sufficient tricks to make nine impossible, i.e. when they win five tricks.
    *
    * @param t  the value of T.
    * @param to an optional T which represents an achieved goal state
    * @return an Either of Boolean or Seq[T].
    *         If the return is Right(Seq(...)) then the content of the option is the list of (new) children.
    *         If the result is Right(Nil), it signifies that the given value of t holds no promise and therefore should not be further expanded.
    *         If the return is Left(true), it signifies that we have reached the (positive) goal.
    *         If the return is Left(false), it signifies that we have reached the (negative) goal.
    */
  def result(t: T, to: Option[T]): Either[Boolean, Seq[T]] = decide(t) match {
    case Some(b) => Left(b)
    case None => Right(if (canDecide(t, to)) successors(t) else Nil)
  }

}

case class ExpandingNodeException(str: String) extends Exception(str)

