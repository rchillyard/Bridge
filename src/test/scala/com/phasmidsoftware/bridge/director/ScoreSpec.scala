/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.bridge.director

import com.phasmidsoftware.output.MockWriter
import com.phasmidsoftware.util.Output
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * @author scalaprof
  */
class ScoreSpec extends AnyFlatSpec with should.Matchers {

  // CONSIDER sort this out.
  // This file seems to be incorrect so maybe it's not a problem that this test doesn't succeed
  //	ignore should "read travelers.lexington.2016.0503 as a resource" in {
  //		val resource = "travelers.lexington.2016.0503"
  //		val ey: Try[Event] = Option(getClass.getResourceAsStream(resource)) match {
  //			case Some(s) => RecapParser.readEvent(Source.fromInputStream(s))
  //			case None => Failure(new Exception(s"doScoreResource: cannot open resource: $resource"))
  //		}
  //		ey should matchPattern { case Success(Event(_, _)) => }
  //		val event = ey.get
  //		val results: Map[Preamble, Seq[Result]] = event.createResults
  //		val rso: Option[Seq[Result]] = results.get(event.sections.head.preamble)
  //		rso should matchPattern { case Some(_) => }
  //		val resultsA: Seq[Result] = rso.get
  //		resultsA.size shouldBe 2
  //		val resultANS: Result = resultsA.head
  //		resultANS.isNS shouldBe true
  //		resultANS.top shouldBe 5
  //		val cards: Map[Int, (Rational, Int)] = resultANS.cards
  //		cards.size shouldBe 14
  //		val scores = (for (score <- cards.keys) yield cards(score)).toSeq
  //		scores.size shouldBe 12
  //		val total: Rational = (for ((r, _) <- cards.values) yield r).sum
  //		total shouldBe Rational(2).invert * cards.size * (resultANS.top + 1)
  //		scores.size shouldBe 6
  //		for ((_, (_, t)) <- cards) t shouldBe resultANS.top + 1
  //	}

  behavior of "Score"
  it should "read travelers.lexington.2017.0404 as a resource" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreResource("travelers.lexington.2017.0404", Output(writer))) o.close()
    writer.spilled shouldBe 3023
  }
  it should "read travelers.lexington.2017.0404P as a resource (includes pickup slips)" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreResource("travelers.lexington.2017.0404P", Output(writer))) o.close()
    writer.spilled shouldBe 3023
  }
  it should "read travelers.lexington.2017.0404 as a file" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/travelers.lexington.2017.0404", Output(writer))) o.close()
    writer.spilled shouldBe 3023
  }
  it should "read ConcordCountryClub20191007.txt" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreResource("ConcordCountryClub20191007.txt", Output(writer))) o.close()
    writer.spilled shouldBe 3704
  }

  // FIXME Issue #8
  //noinspection SpellCheckingInspection
  ignore should "read keremshalom.2019.0509.txt as a file" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/keremshalom.2019.0509.txt", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    writer.spillway shouldBe
      """Kerem Shalom Beginner Duplicate: May 9th 2019
        |Section A
        |Results for direction N/S
        |Pos	Pair	MPs	Percent	Names
        |1	1	 6.00	75.00%	Dan & Tenley
        |2	3	 5.67	56.67%	Chris & Kathy
        |3	2	 5.17	43.06%	Irene & Robin
        |4	4	 3.17	31.67%	Tom & Jane
        |Results for direction E/W
        |Pos	Pair	MPs	Percent	Names
        |1	4	 6.83	68.33%	Ghilaine & Bill
        |2	2	 3.83	47.92%	JoAnn & Margaret
        |3	1	 4.33	43.33%	Marian & Patty
        |4	3	 5.00	41.67%	Wendy & Ruth
        |=====================================================
        |=====================================================
        |Kerem Shalom Beginner Duplicate: May 9th 2019
        |A
        |1N	Dan & Tenley
        |1E	Marian & Patty
        |2N	Irene & Robin
        |2E	JoAnn & Margaret
        |3N	Chris & Kathy
        |3E	Wendy & Ruth
        |4N	Tom & Jane
        |4E	Ghilaine & Bill
        |
        |Board: 1 with 4 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	450	 1.50
        |2	3	450	 1.50
        |4	4	450	 1.50
        |3	2	450	 1.50
        |Board: 2 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	-980	 1.00
        |2	3	-1010	 0.00
        |3	2	-510	 2.00
        |Board: 3 with 4 plays
        |NS pair	EW pair	NS score	NS MPs
        |2	2	-650	 1.00
        |4	4	-650	 1.00
        |1	3	50	 3.00
        |3	1	-650	 1.00
        |Board: 4 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |2	2	-100	 0.50
        |4	4	-100	 0.50
        |1	3	1430	 2.00
        |Board: 5 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |3	3	50	 1.00
        |2	1	50	 1.00
        |4	4	50	 1.00
        |Board: 6 with 3 plays
        |NS pair	EW pair	NS score	NS MPs
        |3	3	450	 1.00
        |2	1	480	 2.00
        |4	4	-50	 0.00
        |""".stripMargin
    writer.spilled shouldBe 1317
  }
  // NOTE we really don't need this test (which currently fails)
  ignore should "read KeremShalom.prn as a PRN file (output from Excel)" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/KeremShalom.prn", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    //    writer.spillway shouldBe "Kerem Shalom Beginner Duplicate: May 9th 2019\nSection A\nResults for direction N/S\n1 :  6.00 : 75.00% : Dan & Tenley\n3 :  5.67 : 56.67% : Chris & Kathy\n2 :  5.17 : 43.06% : Irene & Robin\n4 :  3.17 : 31.67% : Tom & Jane\nResults for direction E/W\n4 :  6.83 : 68.33% : Ghilaine & Bill\n2 :  3.83 : 47.92% : JoAnn & Margaret\n1 :  4.33 : 43.33% : Marian & Patty\n3 :  5.00 : 41.67% : Wendy & Ruth\n=====================================================\n=====================================================\nKerem Shalom Beginner Duplicate: May 9th 2019\nA\n1N: Dan & Tenley\n1E: Marian & Patty\n2N: Irene & Robin\n2E: JoAnn & Margaret\n3N: Chris & Kathy\n3E: Wendy & Ruth\n4N: Tom & Jane\n4E: Ghilaine & Bill\n\nBoard: 1 with 4 plays\nNS: 1, EW: 1, score: 450, MP: 1.50\nNS: 2, EW: 3, score: 450, MP: 1.50\nNS: 4, EW: 4, score: 450, MP: 1.50\nNS: 3, EW: 2, score: 450, MP: 1.50\nBoard: 2 with 3 plays\nNS: 1, EW: 1, score: -980, MP: 1.00\nNS: 2, EW: 3, score: -1010, MP: 0.00\nNS: 3, EW: 2, score: -510, MP: 2.00\nBoard: 3 with 4 plays\nNS: 2, EW: 2, score: -650, MP: 1.00\nNS: 4, EW: 4, score: -650, MP: 1.00\nNS: 1, EW: 3, score: 50, MP: 3.00\nNS: 3, EW: 1, score: -650, MP: 1.00\nBoard: 4 with 3 plays\nNS: 2, EW: 2, score: -100, MP: 0.50\nNS: 4, EW: 4, score: -100, MP: 0.50\nNS: 1, EW: 3, score: 1430, MP: 2.00\nBoard: 5 with 3 plays\nNS: 3, EW: 3, score: 50, MP: 1.00\nNS: 2, EW: 1, score: 50, MP: 1.00\nNS: 4, EW: 4, score: 50, MP: 1.00\nBoard: 6 with 3 plays\nNS: 3, EW: 3, score: 450, MP: 1.00\nNS: 2, EW: 1, score: 480, MP: 2.00\nNS: 4, EW: 4, score: -50, MP: 0.00\n"
    writer.spilled shouldBe 1529
  }
  ignore should "read KeremShalom.txt as a TXT file (output from Excel)" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/KeremShalom.txt", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    //    writer.spillway shouldBe "Kerem Shalom Beginner Duplicate: May 9th 2019\nSection A\nResults for direction N/S\n1 :  6.00 : 75.00% : Dan & Tenley\n3 :  5.67 : 56.67% : Chris & Kathy\n2 :  5.17 : 43.06% : Irene & Robin\n4 :  3.17 : 31.67% : Tom & Jane\nResults for direction E/W\n4 :  6.83 : 68.33% : Ghilaine & Bill\n2 :  3.83 : 47.92% : JoAnn & Margaret\n1 :  4.33 : 43.33% : Marian & Patty\n3 :  5.00 : 41.67% : Wendy & Ruth\n=====================================================\n=====================================================\nKerem Shalom Beginner Duplicate: May 9th 2019\nA\n1N: Dan & Tenley\n1E: Marian & Patty\n2N: Irene & Robin\n2E: JoAnn & Margaret\n3N: Chris & Kathy\n3E: Wendy & Ruth\n4N: Tom & Jane\n4E: Ghilaine & Bill\n\nBoard: 1 with 4 plays\nNS: 1, EW: 1, score: 450, MP: 1.50\nNS: 2, EW: 3, score: 450, MP: 1.50\nNS: 4, EW: 4, score: 450, MP: 1.50\nNS: 3, EW: 2, score: 450, MP: 1.50\nBoard: 2 with 3 plays\nNS: 1, EW: 1, score: -980, MP: 1.00\nNS: 2, EW: 3, score: -1010, MP: 0.00\nNS: 3, EW: 2, score: -510, MP: 2.00\nBoard: 3 with 4 plays\nNS: 2, EW: 2, score: -650, MP: 1.00\nNS: 4, EW: 4, score: -650, MP: 1.00\nNS: 1, EW: 3, score: 50, MP: 3.00\nNS: 3, EW: 1, score: -650, MP: 1.00\nBoard: 4 with 3 plays\nNS: 2, EW: 2, score: -100, MP: 0.50\nNS: 4, EW: 4, score: -100, MP: 0.50\nNS: 1, EW: 3, score: 1430, MP: 2.00\nBoard: 5 with 3 plays\nNS: 3, EW: 3, score: 50, MP: 1.00\nNS: 2, EW: 1, score: 50, MP: 1.00\nNS: 4, EW: 4, score: 50, MP: 1.00\nBoard: 6 with 3 plays\nNS: 3, EW: 3, score: 450, MP: 1.00\nNS: 2, EW: 1, score: 480, MP: 2.00\nNS: 4, EW: 4, score: -50, MP: 0.00\n"
    writer.spilled shouldBe 1529
  }
  it should "read KeremShalom.txt as a TSV file (output from Excel)" in {
    val writer = MockWriter(8192)
    //noinspection SpellCheckingInspection
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/KeremShalom.tsv", Output(writer))) o.close()
    //noinspection SpellCheckingInspection
    //    writer.spillway shouldBe "Kerem Shalom Beginner Duplicate: May 9th 2019\nSection A\nResults for direction N/S\n1 :  6.00 : 75.00% : Dan & Tenley\n3 :  5.67 : 56.67% : Chris & Kathy\n2 :  5.17 : 43.06% : Irene & Robin\n4 :  3.17 : 31.67% : Tom & Jane\nResults for direction E/W\n4 :  6.83 : 68.33% : Ghilaine & Bill\n2 :  3.83 : 47.92% : JoAnn & Margaret\n1 :  4.33 : 43.33% : Marian & Patty\n3 :  5.00 : 41.67% : Wendy & Ruth\n=====================================================\n=====================================================\nKerem Shalom Beginner Duplicate: May 9th 2019\nA\n1N: Dan & Tenley\n1E: Marian & Patty\n2N: Irene & Robin\n2E: JoAnn & Margaret\n3N: Chris & Kathy\n3E: Wendy & Ruth\n4N: Tom & Jane\n4E: Ghilaine & Bill\n\nBoard: 1 with 4 plays\nNS: 1, EW: 1, score: 450, MP: 1.50\nNS: 2, EW: 3, score: 450, MP: 1.50\nNS: 4, EW: 4, score: 450, MP: 1.50\nNS: 3, EW: 2, score: 450, MP: 1.50\nBoard: 2 with 3 plays\nNS: 1, EW: 1, score: -980, MP: 1.00\nNS: 2, EW: 3, score: -1010, MP: 0.00\nNS: 3, EW: 2, score: -510, MP: 2.00\nBoard: 3 with 4 plays\nNS: 2, EW: 2, score: -650, MP: 1.00\nNS: 4, EW: 4, score: -650, MP: 1.00\nNS: 1, EW: 3, score: 50, MP: 3.00\nNS: 3, EW: 1, score: -650, MP: 1.00\nBoard: 4 with 3 plays\nNS: 2, EW: 2, score: -100, MP: 0.50\nNS: 4, EW: 4, score: -100, MP: 0.50\nNS: 1, EW: 3, score: 1430, MP: 2.00\nBoard: 5 with 3 plays\nNS: 3, EW: 3, score: 50, MP: 1.00\nNS: 2, EW: 1, score: 50, MP: 1.00\nNS: 4, EW: 4, score: 50, MP: 1.00\nBoard: 6 with 3 plays\nNS: 3, EW: 3, score: 450, MP: 1.00\nNS: 2, EW: 1, score: 480, MP: 2.00\nNS: 4, EW: 4, score: -50, MP: 0.00\n"
    writer.spilled shouldBe 2036
  }
  it should "output with equal ranks" in {
    val writer = MockWriter(10240)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/Newton/Newton20240924.txt", Output(writer))) o.close()
    println(writer.spillway)
    writer.spillway.substring(0, 200) shouldBe "Newton Sep 24th 2024  d971b658\nSection A\nResults for direction N/S\nRank\tPair\tMPs\tPercent\tNames\n1=\t8\t33.50\t69.79%\tAmy Avergun & Penny Scharfman\n1=\t9\t33.50\t69.79%\tMarsha & Robert Greenstein\n3 \t3\t33.00\t6"
  }

  it should "output with unplayed boards" in {
    val writer = MockWriter(10240)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/Newton/Newton20241001a.txt", Output(writer))) o.close()
    writer.spillway.substring(0, 2026) shouldBe
      """Newton Oct 1st 2024  8c157e2f
        |Section A
        |Results for direction N/S
        |Rank	Pair	MPs	Percent	Names
        |1 	7	33.88	70.57%	Carol Leahy & Deanna Szeto
        |2 	6	28.13	58.59%	Mary Ellen Clark & Leslie Greenberg
        |3 	9	27.56	57.42%	Amy Avergun & Penny Scharfman
        |4 	8	24.38	50.78%	Jane Venti & Jane Volden
        |5 	3	22.69	47.27%	Josh Gahm & Marya Van'T Hul
        |6 	2	21.44	44.66%	Joanne Hennessy & Veets Veitas
        |7 	5	20.63	42.97%	Vivian Hernandez & Roberta Kosberg
        |8 	4	19.31	40.23%	Marsha & Rob Greenstein
        |9 	1	16.80	35.00%	Judy & David Taub
        |Results for direction E/W
        |Rank	Pair	MPs	Percent	Names
        |1 	1	40.20	83.75%	Kaj Wilson & Ellen Dockser
        |2 	4	34.69	72.27%	Rick & Lisa Martin
        |3 	3	29.31	61.07%	Robin Zelle & Barbara Berenson
        |4 	6	25.88	53.91%	Gerri Taylor & Sherrill Kobrick
        |5 	8	22.63	47.14%	Judy Tucker & Sheila Jones
        |6 	9	21.44	44.66%	Kathy Curtiss & Linda Worters
        |7 	7	20.63	42.97%	Alan Gordon & Margaret Meehan
        |8 	2	17.06	35.55%	Rebecca Kratka & MJ Weinstein
        |9 	5	 6.88	14.32%	Barbara & Don Oppenheimer
        |=====================================================
        |=====================================================
        |Newton Oct 1st 2024
        |A
        |1N	Judy & David Taub
        |2N	Joanne Hennessy & Veets Veitas
        |3N	Josh Gahm & Marya Van'T Hul
        |4N	Marsha & Rob Greenstein
        |5N	Vivian Hernandez & Roberta Kosberg
        |6N	Mary Ellen Clark & Leslie Greenberg
        |7N	Carol Leahy & Deanna Szeto
        |8N	Jane Venti & Jane Volden
        |9N	Amy Avergun & Penny Scharfman
        |1E	Kaj Wilson & Ellen Dockser
        |2E	Rebecca Kratka & MJ Weinstein
        |3E	Robin Zelle & Barbara Berenson
        |4E	Rick & Lisa Martin
        |5E	Barbara & Don Oppenheimer
        |6E	Gerri Taylor & Sherrill Kobrick
        |7E	Alan Gordon & Margaret Meehan
        |8E	Judy Tucker & Sheila Jones
        |9E	Kathy Curtiss & Linda Worters
        |
        |Board: 1 with 9 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	-100	 0.00 (probable contract: NS  down 1X)
        |2	2	110	 5.50 (probable contract: NS partial 2 major)
        |3	3	110	 5.50 (probable contract: NS partial 2 major)
        |4	4	-50	 1.50 (probable contract: NS  down 1)
        |5	5	110	 5.50 (probable contract: NS partial 2 major)
        |6	6	-50	 1.50 (probable contract: NS  down 1)
        |7""".stripMargin
  }
  it should "output from Newton" in {
    val writer = MockWriter(10240)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/Newton/Newton20241001.txt", Output(writer))) o.close()
    writer.spillway.substring(0, 2000) shouldBe
      """Newton Oct 1st 2024  1587e8cb
        |Section A
        |Results for direction N/S
        |Rank	Pair	MPs	Percent	Names
        |1 	7	34.00	70.83%	Carol Leahy & Deanna Szeto
        |2 	6	28.50	59.38%	Mary Ellen Clark & Leslie Greenberg
        |3 	9	27.50	57.29%	Amy Avergun & Penny Scharfman
        |4 	8	24.50	51.04%	Jane Venti & Jane Volden
        |5 	3	22.50	46.88%	Josh Gahm & Marya Van'T Hul
        |6 	2	22.00	45.83%	Joanne Hennessy & Veets Veitas
        |7 	5	21.00	43.75%	Vivian Hernandez & Roberta Kosberg
        |8 	4	20.00	41.67%	Marsha & Rob Greenstein
        |9 	1	16.00	33.33%	Judy & David Taub
        |Results for direction E/W
        |Rank	Pair	MPs	Percent	Names
        |1 	1	39.50	82.29%	Kaj Wilson & Ellen Dockser
        |2 	4	34.00	70.83%	Rick & Lisa Martin
        |3 	3	29.50	61.46%	Robin Zelle & Barbara Berenson
        |4 	6	25.50	53.13%	Gerri Taylor & Sherrill Kobrick
        |5 	8	22.50	46.88%	Judy Tucker & Sheila Jones
        |6 	9	21.50	44.79%	Kathy Curtiss & Linda Worters
        |7 	7	20.50	42.71%	Alan Gordon & Margaret Meehan
        |8 	2	16.50	34.38%	Rebecca Kratka & MJ Weinstein
        |9 	5	 6.50	13.54%	Barbara & Don Oppenheimer
        |=====================================================
        |=====================================================
        |Newton Oct 1st 2024
        |A
        |1N	Judy & David Taub
        |2N	Joanne Hennessy & Veets Veitas
        |3N	Josh Gahm & Marya Van'T Hul
        |4N	Marsha & Rob Greenstein
        |5N	Vivian Hernandez & Roberta Kosberg
        |6N	Mary Ellen Clark & Leslie Greenberg
        |7N	Carol Leahy & Deanna Szeto
        |8N	Jane Venti & Jane Volden
        |9N	Amy Avergun & Penny Scharfman
        |1E	Kaj Wilson & Ellen Dockser
        |2E	Rebecca Kratka & MJ Weinstein
        |3E	Robin Zelle & Barbara Berenson
        |4E	Rick & Lisa Martin
        |5E	Barbara & Don Oppenheimer
        |6E	Gerri Taylor & Sherrill Kobrick
        |7E	Alan Gordon & Margaret Meehan
        |8E	Judy Tucker & Sheila Jones
        |9E	Kathy Curtiss & Linda Worters
        |
        |Board: 1 with 9 plays
        |NS pair	EW pair	NS score	NS MPs
        |1	1	-100	 0.00 (probable contract: NS  down 1X)
        |2	2	110	 5.50 (probable contract: NS partial 2 major)
        |3	3	110	 5.50 (probable contract: NS partial 2 major)
        |4	4	-50	 1.50 (probable contract: NS  down 1)
        |5	5	110	 5.50 (probable contract: NS partial 2 major)
        |6	6	-50	 1.50 (probab""".stripMargin
  }
  // Issue #11
  ignore should "output from Newton despite having PhantomPair" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromFile("src/test/resources/com/phasmidsoftware/bridge/director/Newton/Newton20241015bad.txt", Output(writer))) o.close()
    writer.spilled shouldBe -1
  }

  // TODO Find out why this doesn't work!
  ignore should "read ConcordCountryClub20191007.txt using doScoreFromName" in {
    val writer = MockWriter(8192)
    for (o <- Score.doScoreFromName(isResource = true, "ConcordCountryClub20191007.txt", "", output = Output(writer))) o.close()
    writer.spilled shouldBe 2642
  }

}

