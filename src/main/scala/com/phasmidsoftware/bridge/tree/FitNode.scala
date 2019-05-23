package com.phasmidsoftware.bridge.tree

trait Fitness[X] {

	/**
		* Method to evaluate the fitness of an X.
		*
		* @param x the value whose fitness is to be evaluated.
		* @return the fitness of x as a Double.
		*/
	def fitness(x: X): Double

}

abstract class FitNode[X: Fitness](val t: X, val isTerminal: Boolean, val children: Seq[Node[X]]) extends Node[X] with Ordered[FitNode[X]] {

	def compare(that: FitNode[X]): Int = {
		val xf = implicitly[Fitness[X]]
		implicitly[Ordering[Double]].compare(xf.fitness(t), xf.fitness(that.t))
	}
}

object FitNode {
}

case class FitNodeException(str: String) extends Exception(str)
