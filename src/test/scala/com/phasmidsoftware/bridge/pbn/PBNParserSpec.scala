/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.pbn

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Success

/**
	* @author scalaprof
	*/
class PBNParserSpec extends FlatSpec with Matchers {
	val parser = new PBNParser

	behavior of "endOfLine"
	it should "parse endOfLine" in {
		parser.parseAll(parser.endOfLine, "; \n") should matchPattern { case parser.Success(_, _) => }
	}

	behavior of "emptySpace"
	it should "parse emptySpace" in {
		parser.parseAll(parser.emptySpace, "; \n") should matchPattern { case parser.Success(_, _) => }
		parser.parseAll(parser.emptySpace, " ; \n") should matchPattern { case parser.Success(_, _) => }
		parser.parseAll(parser.emptySpace, " ") should matchPattern { case parser.Success(_, _) => }
	}

	behavior of "digits"
	it should "parse 99 with digits(2)" in {
		parser.parseAll(parser.digits(2), "99") should matchPattern { case parser.Success(_, _) => }
	}

	it should "not parse 99 with digits(4)" in {
		parser.parseAll(parser.digits(4), "99") should matchPattern { case parser.Failure(_, _) => }
	}

	behavior of "setValue"
	it should "parse Lexington;Z_&Y" in {
		parser.parseAll(parser.set, "Lexington;Z_&Y") should matchPattern { case parser.Success(SetValue(_), _) => }
	}

	behavior of "stringValue"
	it should "parse Z_&Y" in {
		parser.parseAll(parser.stringValue, "Z_&Y") should matchPattern { case parser.Success(StringValue(_), _) => }
	}

	it should "parse Lexington" in {
		parser.parseAll(parser.stringValue, "Lexington") should matchPattern { case parser.Success(StringValue(_), _) => }
	}

	behavior of "holding"
	it should "parse AKJ" in {
		parser.parseAll(parser.holding, "AKJ") should matchPattern { case parser.Success(_, _) => }
	}

	behavior of "hand"
	it should "parse AKJ.Q952.73.T864" in {
		parser.parseAll(parser.hand, "AKJ.Q952.73.T864") should matchPattern { case parser.Success(_, _) => }
	}

	behavior of "deal"
	it should "parse N:T7.AKJ5.K83.KQJ3 K.T9832.A962.AT8 QJ9653.6.754.742 A842.Q74.QJT.965" in {
		parser.parseAll(parser.deal, "N:T7.AKJ5.K83.KQJ3 K.T9832.A962.AT8 QJ9653.6.754.742 A842.Q74.QJT.965") should
			matchPattern { case parser.Success(DealValue(_, _), _) => }
	}

	behavior of "date"
	it should "parse 2019.06.02" in {
		parser.parseAll(parser.date, "2019.06.02") should matchPattern { case parser.Success(DateValue(_, _, _), _) => }
	}

	behavior of "name"
	it should "parse Site" in {
		parser.parseAll(parser.name, "Site") should matchPattern { case parser.Success(Name(_), _) => }
	}

	behavior of "tagPair"
	it should "parse [Date \"2016.01.28\"]" in {
		parser.parseAll(parser.tagPair, "[Date \"2016.01.28\"]") should matchPattern { case parser.Success(_, _) => }
	}

	it should "parse [Site \"Lexington\"]" in {
		parser.parseAll(parser.tagPair, "[Site \"Lexington\"]") should matchPattern { case parser.Success(_, _) => }
	}

	behavior of "game"
	it should "parse two tagPairs" in {
		parser.parseAll(parser.game, "[Site \"Lexington\"][Date \"2016.01.28\"]") should matchPattern { case parser.Success(_, _) => }
	}

	behavior of "PBN"
	it should "parse two games" in {
		parser.parseAll(parser.pbn, "[Site \"Lexington\"][Date \"2016.01.28\"][Board \"1\"]\n[Site \"Lexington\"][Date \"2016.01.28\"][Board \"2\"]") should
			matchPattern { case parser.Success(_, _) => }
	}

	behavior of "ParsePBN"
	it should "parse com/phasmidsoftware/bridge/director/LEXINGTON 2016.2.9.PBN" in {
		val source = Source.fromResource("com/phasmidsoftware/bridge/director/LEXINGTON 2016.2.9.PBN")
		val pbn = PBNParser.parsePBN(source)
		pbn should matchPattern { case Success(_) => }
		pbn.get.games.size shouldBe 8
	}
}

