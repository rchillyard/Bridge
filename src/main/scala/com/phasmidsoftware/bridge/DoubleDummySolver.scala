/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge

import com.phasmidsoftware.bridge.cards.Whist
import com.phasmidsoftware.bridge.pbn.{PBN, PBNParser}
import com.phasmidsoftware.gambit.util.LazyLogger

import scala.util.Success

/**
  * CLI entry point: parses a PBN file and runs the object-graph engine's "makable contracts"
  * analysis (`Game.analyzeMakableContracts`, via `Deal.analyzeContracts`) against one board in
  * it.
  *
  * Deliberately lives at the bare `com.phasmidsoftware.bridge` package root, above `cards`/
  * `pbn`, rather than alongside `Whist` (as it used to) -- it needs `pbn` (to parse the file)
  * as well as `cards` (for `Whist`), and `pbn` already depends on `cards`, so putting the
  * entry point in `cards` (where it previously lived) forced `cards` to depend on `pbn` right
  * back, a real (if harmless-to-compile) package cycle.
  */
@main def doubleDummySolver(args: String*): Unit = {
  import scala.io.{Codec, Source}
  val logger = LazyLogger(classOf[Whist])
  assert(args.nonEmpty, "At least one argument required")
  val filename = args(0)
  val maybeBoard = args.lift(1).flatMap(_.toIntOption)

  given Codec = Codec.UTF8

  PBNParser.parsePBN(Source.fromFile(filename)) match {
    case Success(PBN(games)) =>
      (for (x <- maybeBoard; g <- games.lift(x)) yield g) match {
        case Some(g) =>
          g.analyzeMakableContracts()
        case None =>
          logger.error(s"No game to parse PBN file: $filename ($maybeBoard)")
      }
    case _ =>
      logger.error(s"Failed to parse PBN file: $filename")
  }
}
