/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge

import org.scalatest.Tag

/**
  * Marks a test that takes more than ~30 seconds to run on its own. Mostly found in `src/it`
  * (real deals, larger synthetic end positions, cross-engine validation), but applies wherever
  * a test earns it.
  *
  * Exclude these for a fast run: `sbt "IT/testOnly * -- -l com.phasmidsoftware.bridge.SlowTest"`.
  * Run only these: `sbt "IT/testOnly * -- -n com.phasmidsoftware.bridge.SlowTest"`.
  */
object SlowTest extends Tag("com.phasmidsoftware.bridge.SlowTest")
