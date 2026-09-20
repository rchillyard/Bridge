/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.bridge.cards

/**
  * A single contract to analyze: the leader, strain, target trick count, and declarer for one
  * entry of a PBN `OptimumResultTable` (see `pbn.Game.makableContracts`, which parses these
  * out). Lives in `cards` (rather than `pbn`, where it originated) because it's consumed by
  * both engines' "makable contracts" entry points -- `Deal.analyzeContracts` (object-graph)
  * and `BitAnalysis.analyzeMakableContracts` (bitboard) -- alongside the other domain types
  * (`Suit`, `Deal`, ...) both already share.
  *
  * @param leader   the index of the player on opening lead.
  * @param strain   the trump suit, or `None` for notrump.
  * @param tricks   the number of tricks the declarer needs to make the contract.
  * @param declarer the index of the declarer.
  */
case class Contract(leader: Int, strain: Option[Suit], tricks: Int, declarer: Int)

/**
  * The trump suit (if any) for a deal, represented as an optional suit INDEX (0..3, matching
  * `SuitMask`/`DealBits`' bit-position convention) rather than a `Suit` itself -- the bitboard
  * engine's internal representation, converted from a real `Suit` at the boundary (see
  * `BitConversions.toStrainIndex` and `BitAnalysis.analyzeDoubleDummy`'s `Suit`-accepting
  * overload). `None` means notrump.
  *
  * @param suit the trump suit index (0..3), or `None` for notrump.
  */
case class Strain(suit: Option[Int]):
  /** e.g. "S", "H", "D", "C", or "NT" for notrump. */
  override def toString: String = suit.flatMap(s => "SHDC".toIndexedSeq.lift(s)).map(c => c.toString).getOrElse("NT")

  /** True if `s` is a plain (non-trump) suit under this strain. */
  def isPlainSuit(s: Int): Boolean = !isTrump(s)

  /** True if `s` is the trump suit under this strain (always `false` for notrump). */
  def isTrump(s: Int): Boolean = suit.contains(s)

  /** True if this strain is notrump (no trump suit). */
  def isNoTrump: Boolean = suit.isEmpty

  /** True if there is a trump suit and it satisfies `p`. */
  def matches(p: Int => Boolean): Boolean = suit.exists(p)

/**
  * Constructors/constants for `Strain`, mirroring `Suit`'s own `apply(Char)` convention.
  */
object Strain:
  /** A suit-contract strain, given its suit index (0..3). */
  def apply(n: Int): Strain = Strain(Some(n))

  /** A suit-contract strain, given the real `Suit`. */
  def apply(s: Suit): Strain = apply(s.priority)

  /** A suit-contract strain, given the suit's letter ('S'/'H'/'D'/'C'). */
  def apply(x: Char): Strain = apply(Suit(x))

  val NoTrump = apply(None)
  val Spades = apply('S')
  val Hearts = apply('H')
  val Diamonds = apply('D')
  val Clubs = apply('C')
