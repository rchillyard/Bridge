/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards.bits

import com.phasmidsoftware.bridge.cards.{Deal, Suit}

/**
  * The one place this new bitboard engine touches the existing `Deal`/`Suit` object
  * model: converting an initial `Deal` (and trump `Suit`) into `DealBits` at the root of
  * a search. Nothing on the hot path (`BitState.legalPlays`/`play`) depends on this.
  */
object BitConversions:

  /** `Suit.priority` already runs 0..3 (Spades, Hearts, Diamonds, Clubs) -- reuse it directly. */
  def suitIndex(suit: Suit): Int = suit.priority

  def toStrainIndex(strain: Option[Suit]): Option[Int] = strain.map(suitIndex)

  def toDealBits(deal: Deal): DealBits =
    def handBitsFor(h: Int): HandBits =
      val hand = deal.holdings(h)
      hand.foldLeft(HandBits.empty) { case (acc, (suit, holding)) =>
        holding.cards.foldLeft(acc)((a, c) => a.setCard(suitIndex(suit), SuitMask.RanksPerSuit - 1 - c.rank.priority))
      }
    DealBits(handBitsFor(0), handBitsFor(1), handBitsFor(2), handBitsFor(3))
