/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

import com.typesafe.config.ConfigFactory

/**
  * Runtime-configurable tuning values, backed by `application.conf` (see
  * `src/main/resources/application.conf`). `ConfigFactory.load()` already lets any of
  * these be overridden without touching the file, e.g.
  * `-Dbridge.transposition-table.max-size=1500000`.
  */
object BridgeConfig:
  private val config = ConfigFactory.load()

  /**
    * Maximum number of entries a double-dummy search's transposition table will hold
    * before it stops accepting new ones (`FlatTTCache.maxSize`). Bounds heap usage on
    * hard positions that would otherwise grow the table without limit; once full, a
    * store is simply a no-op (a cache miss later, not an error) -- this only ever costs
    * some redundant re-computation, never correctness.
    */
  val ttMaxSize: Int = config.getInt("bridge.transposition-table.max-size")
