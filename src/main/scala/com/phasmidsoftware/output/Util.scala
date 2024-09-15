/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.output

import java.io.IOException
import java.util.Objects.isNull
import scala.io.Source
import scala.util._

object Util {
  /**
    * Convert a doubleton sequence into a Tuple2.
    *
    * @param xs a sequence of Xs.
    * @tparam X the underlying type.
    * @return an (X,X)
    */
  def asTuple2[X](xs: Seq[X]): (X, X) =
    if (xs.length == 2) xs.head -> xs.last
    else throw UtilException(s"cannot convert sequence to Tuple2 because cardinality is wrong: ${xs.length}")
}

case class UtilException(s: String) extends Exception(s)

object Using {
  def apply[R, T](r: R)(f: R => Try[T]): Try[T] = {
    val rt: Try[R] = Try(r) match {
      case Success(x) if isNull(x) => Failure(new IOException("resource returned is null"))
      case q@Success(_) => q
      case q => q
    }
    val result: Try[T] = rt flatMap f
    rt map {
      case s: Source => s.close()
      case a: AutoCloseable => a.close()
    }
    result
  }
}