package com.phasmidsoftware.bridge.cards

import com.phasmid.laScala.Shuffle
import com.phasmidsoftware.output.{Output, Outputable}

case class Deal(title: String, hs: Seq[Hand]) extends Outputable {
	private val Seq(n, e, s, w) = hs

	def north: Hand = n

	def east: Hand = e

	def south: Hand = s

	def west: Hand = w

	private def outputHand(name: String, hand: Hand): Output = Output(s"$name:\t$hand").insertBreak

	def output(output: Output): Output = (output :+ title).insertBreak ++ outputHand("North", n) ++ outputHand("East", e) ++ outputHand("South", s) ++ outputHand("West", w)
}

object Deal {
	def fromCards(title: String, cs: Seq[Card]): Deal = apply(title, (for (cs: Seq[Card] <- cs.grouped(13)) yield Hand.apply(cs)).toSeq)

	def apply(title: String, seed: Long = System.nanoTime()): Deal = {
		val newDeck: Seq[Card] = for (s <- Seq(Spades, Hearts, Diamonds, Clubs); r <- Seq(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Trey, Deuce)) yield Card(s, r)
		val shuffler = Shuffle[Card](seed)
		fromCards(title, shuffler(newDeck))
	}
}