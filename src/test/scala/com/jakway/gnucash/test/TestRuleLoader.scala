package com.jakway.gnucash.test

import com.jakway.gnucash.rules.{Loader, UnlinkedTransactionRule}
import org.json4s.jackson.JsonMethods._
import org.scalatest.{FlatSpec, Matchers}

class TestRuleLoader extends FlatSpec with Matchers {
  import org.json4s._

  implicit val formats = DefaultFormats

  object TestObjects {
    import org.json4s.JsonDSL._

    val oneRuleJson: JValue = (Loader.jsonRoot ->
      ("foo" ->
        ("pattern" -> ".*") ~
        ("priority" -> 1) ~
        ("sourceAccount" -> "bar") ~
        ("destAccount" -> "baz"))
      )

    val oneRuleExpected = Seq(UnlinkedTransactionRule(
      ".*", 1.toDouble.toString, "bar", "baz"))

    lazy val oneRuleCommented = insertComments("//examplecomment")(pretty(render(oneRuleJson)))

  }
  import TestObjects._

  def insertComments(comment: String)(js: String): Seq[String] = {
    val ljs = js.lines

    //insert a comment in the middle of the Json

    val as = js.lines.take(ljs.length / 2)
    val bs = js.lines.drop(ljs.length / 2)

    val middle = as ++ Seq(comment) ++ bs

    val beginning = Seq(comment) ++ js.lines
    val end = js.lines ++ Seq(comment)

    Seq(beginning, middle, end.toSeq).map(_.reduce(_ ++ "\n" ++ _))
  }

  "The Transaction Rule Loader" should "load a single transaction" in {
    new Loader(compact(render(oneRuleJson))).parse shouldEqual
      Right(oneRuleExpected)
  }

  it should "handle comments" in {
    println(pretty(render(oneRuleJson)))
    println(oneRuleCommented.toString())
    oneRuleCommented.map(new Loader(_).parse) shouldEqual (0 to 2).toList.map(_ => Right(oneRuleExpected))
  }
}
