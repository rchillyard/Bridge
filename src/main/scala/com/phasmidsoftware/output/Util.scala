package com.phasmidsoftware.output

object Util {
	/**
		* Convert a doubleton sequence into a Tuple2.
		*
		* @param xs a sequence of Xs.
		* @tparam X the underlying type.
		* @return an (X,X)
		*/
	def asTuple2[X](xs: Seq[X]): (X, X) = if (xs.length == 2) xs.head -> xs.last else throw new Exception(s"cannot convert sequence to Tuple2 because cardinality is wrong: ${xs.length}")
}
