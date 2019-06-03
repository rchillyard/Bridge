/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.pbn

import com.phasmidsoftware.bridge.cards.Deal


case class PBN(game: Seq[Game]) {
}

case class Game(tagPairs: Seq[(Name, DetailedValue)]) {
	override def toString: String = s"Game: ${tagPairs.toMap}"
}

case class DetailedValue(value: Value, detail: Seq[String]) extends Value {
	override def toString: String = s""""$value"${detail.mkString(" ", ",", "")}"""
}

case class Name(name: String) {
	override def toString: String = name
}

trait Value {
	//	def value: String
}

case class DateValue(year: Int, month: Int, day: Int) extends Value {
	// TODO put into yyy.mm.dd format
	//		def value: LocalDate =
	override def toString: String = ""
}

/**
	* Create a Deal from a sequence of sequence of Strings, each representing a holding.
	*
	* @param q   the compass point of the first hand.
	* @param rss four hands of four holdings in order SHDC.
	* @return a new Deal.
	*/
case class DealValue(q: String, rss: Seq[Seq[String]]) extends Value {
	def deal: Deal = Deal.fromHandStrings("", q, rss)
}

case class SetValue(ws: Seq[String]) extends Value

case class StringValue(value: String) extends Value {
	override def toString: String = value
}
