/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.misc

import com.phasmidsoftware.misc.Predicate.NamedFunction

/**
  * Provides an implicit class to augment functions with naming capabilities, enabling
  * the creation of `NamedFunction` instances through the `^^` operator.
  *
  * NOTE this doesn't do anything yet.
  */
object Named {
  implicit class RawFunction[T, R](f: T => R) {
    def ^^(name: String): NamedFunction[T, R] = NamedFunction(name, f)
  }
}
