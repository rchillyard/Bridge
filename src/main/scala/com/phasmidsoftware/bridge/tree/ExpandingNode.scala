/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.tree

import com.phasmidsoftware.output.{Loggable, Loggables}


/**
  *
  * @param t        the value of this Node.
  * @param so       an optional indication of the solution represented by this sub-tree.
  * @param children the children nodes of this Node.
  * @tparam T the underlying type of the nodes, for which there must be evidence of:
  *           Expandable;
  *           GoalDriven;
  *           Ordering (order is based on the number of "moves" required to reach a given t);
  *           Loggable.
  */
abstract class ExpandingNode[T: Expandable : GoalDriven : Ordering : Loggable]
(val t: T, val so: Option[T], val children: Seq[ExpandingNode[T]]) extends Node[T] {

  // TODO try to be careful that we don't keep re-constructing nodes that are the same.
  import com.phasmidsoftware.output.Flog._

  so match {
    case Some(x) => s"node $t has achieved goal" !! x
    case None =>
  }

  /**
    * Method to add the given node to the children of this Node.
    *
    * CONSIDER: adding the node at the head of the list of children.
    *
    * @param node the node to add as a child.
    * @return a copy of this Node but with node as an additional child, and the so value corresponding to this.
    */
  override def :+(node: Node[T]): ExpandingNode[T] = unit(t, children :+ node)

  /**
    * Method to add the given tns to the children of this Node.
    *
    * @param tns the tns to add as additional children.
    * @return a copy of this Node but with tns as additional childrenand the so value corresponding to this.
    */
  override def ++(tns: Seq[Node[T]]): Node[T] = unit(t, children ++ tns)

  /**
    * Method to add the given x-value to the children of this Node.
    *
    * @param x the x value to be turned into a Node which is then :+'d to this Node.
    * @return a copy of this Node but with x as an additional child value, and the so value corresponding to this.
    */
  override def :+(x: T): ExpandingNode[T] = this :+ unit(x)


  /**
    * Method to form a Node from a T with the given children and with so based on the value in this.
    *
    * @param _t  the given value of T.
    * @param tns the nodes which will be the children of the result.
    * @return a new Node based on t and tns, and the so value corresponding to this.
    */
  def unit(_t: T, tns: Seq[Node[T]]): ExpandingNode[T] = unit(_t, so, tns)

  /**
    * Method to form a Node from a T.
    *
    * @param _t  the given value of T.
    * @param _so an optional T representing a solution.
    * @param tns the nodes which will be the children of the result.
    * @return a new Node based on t and tns.
    */
  def unit(_t: T, _so: Option[T], tns: Seq[Node[T]]): ExpandingNode[T]

  /**
    * Method to expand a branch of a tree, by taking this ExpandingNode and replacing it with child nodes which are themselves recursively expanded.
    * The algorithm operates in a depth-first-search manner.
    *
    * CONSIDER make this tail-recursive
    *
    * @param _so   the currently satisfied goal.
    * @param moves the number of possible moves remaining.
    * @return an Option of ExpandingNode[T].
    */
  def expand(_so: Option[T], moves: Int): Option[ExpandingNode[T]] =
    if (moves < 0)
      None
    else
      implicitly[Expandable[T]].result(t, _so, moves) match {
        // XXX terminating condition found? Mark it.
        case Left(b) =>
          Some(solve(b))
        // XXX no descendants? Signal for this Node to be removed.
        //        case Right(Nil) => None
        // XXX normal situation with descendants? Recursively expand them.
        // TODO we need to fix this because if this becomes goal-satisfied, then the children need to know that.
        // So, we shouldn't convert a state into a node until it's required.
        case Right(ts) =>
          Some(expandSuccessors(ts, moves - 1, _so))
      }


  private def expandSuccessors(ts: Seq[T], moves: Int, _so: Option[T]) = {
    import com.phasmidsoftware.output.Flog._
    _so match {
      case Some(s) => s"expandSuccessors has existing goal: $ts $moves" !! s
      case None =>
    }

    // CONSIDER refactoring as foldLeft

    def getBestSolution(_sor: Option[T]) = _sor match {
      case Some(sr) => Some(_so match {
        case Some(ss) => if (implicitly[Ordering[T]].compare(sr, ss) < 0) sr else ss
        case None => sr
      })
      case None => _so
    }
    //      for (sr <- r.so; ss <- _so) yield if (implicitly[Ordering[T]].compare(sr, ss)<0) sr else ss

    def doExpansion(r: ExpandingNode[T], t: T): ExpandingNode[T] = unit(t, None, Nil).expand(getBestSolution(r.so), moves) match {
      case None => r // expansion came up empty
      case Some(n) => n.so match {
        case Some(g) => r.solve(g) :+ n // goal achieved: add it as a child and mark result as goal achieved
        case None => r
      }
    }

    def inner(r: ExpandingNode[T], work: Seq[T]): ExpandingNode[T] = work match {
      case Nil => r
      case h :: tail => inner(doExpansion(r, h), tail)
    }

    inner(this, ts.toList)
  }

  /**
    * Method to replace node x with node y in this sub-tree.
    * Additionally, if y is decided, then we mark the result as decided.
    *
    * @param x the node to be replaced.
    * @param y the node with which to replace the given node.
    * @return a copy of this Node, but with x replaced by y.
    */
  override def replace(x: Node[T], y: Node[T]): ExpandingNode[T] = y match {
    case yE: ExpandingNode[T] =>
      val result = if (x == yE) this
      else if (children contains x) unit(t, children.map(n => if (n eq x) yE else n)) // NOTE: this picks up the value of so from this
      else unit(t, children map (n => n.replace(x, yE))) // NOTE: this picks up the value of so from this
      yE.so match {
        case Some(b) => result.solve(b)
        case None => result
      }
    case _ => throw NodeException(s"replace cannot operate unless y is an ExpandingNode")
  }

  override def remove(x: Node[T]): ExpandingNode[T] = super.remove(x).asInstanceOf[ExpandingNode[T]]

  /**
    * Construct a new ExpandingNode based on the given value of t and the value of so from this.
    *
    * @param t the given value of T.
    * @return a new non-decided Node based on t, but with no children.
    */
  override def unit(t: T): ExpandingNode[T] = unit(t, so, Nil)

  /**
    * Method to add the given values to the children of this Node.
    *
    * @param ts the values to add as additional child values.
    * @return a copy of this Node but with ts as additional child values.
    */
  override def :+(ts: Seq[T]): ExpandingNode[T] = (this ++ (ts map unit)).asInstanceOf[ExpandingNode[T]]

  /**
    * Method to mark this Node with an optional T (i.e. representing a goal that has been reached).
    *
    * @param success the T value corresponding to the goal reached.
    * @return a new copy of this Node or the same but with so set to Some(success).
    */
  private def solve(success: T) = so match {
    case Some(x) =>
      if (x == success) this
      else if (implicitly[Ordering[T]].compare(x, success) < 0) unit(t, Some(success), children) // TODO check
      else this
    case _ => unit(t, Some(success), children)
  }

//  /**
//    * Method to replace a node of this tree optionally with the given node and to return the resulting tree.
//    *
//    * @param x   the node to be replaced.
//    * @param tno the optional node with which to replace the given node.
//    * @return a copy of this Node, but with node x replaced by
//    *         (1) if tno exists, then its value;
//    *         (2) if tno does not exist, then nothing.
//    */
//  private def replace(x: ExpandingNode[T], tno: Option[ExpandingNode[T]]): ExpandingNode[T] =
//    tno match {
//      case Some(tn) => replace(x, tn)
//      // NOTE: we remove a child that produces no children. The idea is to reduce strain on the GC.
//      case None => remove(x)
//    }

//  /**
//    * Method to replace a node of this tree by expanding it.
//    *
//    * @param x      the node to be, potentially, replaced.
//    * @param moves  the number of possible moves remaining.
//    * @return either the value of solve(goal) where goal is the result of decided; or this tree with x replaced by its expansion.
//    */
//  private def expandAndReplace(x: ExpandingNode[T], moves: Int): ExpandingNode[T] =
//  // NOTE: recursive call to expand
//    x.expand(so, moves) match {
//      case None =>
//        remove(x) // NOTE this should happen only when we've exceeded levels
//      case Some(n) =>
//        n.so match {
//          case Some(b) => replace(x, n) // XXX replace x with n and also mark this as decided
//          case None => remove(x)
//        }
//
//    }
}

object ExpandingNode extends Loggables {

  def expandingNodeLogger[T: Loggable]: Loggable[ExpandingNode[T]] = (t: ExpandingNode[T]) => {

    val wT = implicitly[Loggable[T]].toLog(t.t)
    val wDecided = t.so match {
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
    * A negative goal would be when the antagonist (defending side) wins sufficient tricks to make the protagonist's goal
    * impossible, i.e. when they win five tricks.
    *
    * @param t  the value of T.
    * @param to an optional T which represents an achieved goal state.
    * @param moves the number of possible moves remaining in which to achieve the goal.
    * @return an Either of T or Seq[T].
    *         If the return is Right(Seq(...)) then the content of the option is the list of (new) children.
    *         If the result is Right(Nil), it signifies that the given value of t holds no promise and therefore should not be further expanded.
    *         If the return is Left(T), it signifies that we have reached a solution (goal) represented by the value of T.
    */
  def result(t: T, to: Option[T], moves: Int)(implicit ev: GoalDriven[T]): Either[T, Seq[T]] = {
    import com.phasmidsoftware.output.Flog._
    if (ev.goalAchieved(t)) s"goal achieved" !| Left(t)
    else if (ev.goalOutOfReach(t, to, moves)) s"goal out of reach for $t based on $to and $moves" !| Right(Nil)
    else Right(successors(t))
    //    val decision = if (ev.goalAchieved(t)) Some(Some(t)) else if (ev.goalOutOfReach(t, to)) Some(None) else None
    //    decision match {
    //      case Some(b) =>
    //        b match {
    //          case Some(_t) => Left(_t)
    //          case None => Right(Nil)
    //        }
    //      case None =>
    //        Right(successors(t))
    //    }
  }

}

case class ExpandingNodeException(str: String) extends Exception(str)

trait GoalDriven[T] {
  def goalAchieved(t: T): Boolean

  /**
    * Determine if it is impossible to reach the goal from t when an optional alternative goal (so) has already been reached.
    *
    * @param t     the value of T to test.
    * @param so    if None, then we return false; if Some(s) then we must compare s with t to determine if t can still reach the goal.
    * @param moves the number of remaining moves in which the goal might be reached.
    * @return true if the goal is out of reach, else false.
    */
  def goalOutOfReach(t: T, so: Option[T], moves: Int): Boolean
}

