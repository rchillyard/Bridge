/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

/**
  * The result of a double-dummy analysis. Shared by both engines
  * (`gambit.Whist`, the object-graph engine, and `gambit.bits.BitAnalysis`, the bitboard
  * engine), which is why it lives here in `cards` rather than alongside either engine.
  *
  * - [[DDResult.Exact]]       — full search completed; result is definitive.
  * - [[DDResult.Partial]]     — node limit hit, but one side found a witness line;
  *   result is a qualified best-effort.
  * - [[DDResult.Inconclusive]] — node limit hit before either side found a witness;
  *   no reliable conclusion can be drawn.
  */
enum DDResult:
  case Exact(makes: Boolean, tricks: Int)
  case Partial(makes: Boolean, tricks: Int)
  case Inconclusive
