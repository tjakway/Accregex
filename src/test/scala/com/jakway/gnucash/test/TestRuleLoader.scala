package com.jakway.gnucash.test

import com.jakway.gnucash.rules.{Loader, UnlinkedTransactionRule}
import org.json4s.jackson.JsonMethods._
import org.scalatest.{FlatSpec, Matchers}

class TestRuleLoader extends FlatSpec with Matchers {
  import org.json4s._

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

    val oneRuleCommented = insertComments("foo")(pretty(render(oneRuleJson)))

  }
  import TestObjects._

  def insertComments(comment: String)(js: String): Seq[String] = {
    val ljs = js.lines

    //insert a comment in the middle of the Json
    val middle = ljs.take(ljs.length / 2) ++ Seq(comment) ++ ljs.drop(ljs.length / 2)

    val beginning = Seq(comment) ++ ljs
    val end = ljs ++ Seq(comment)

    Seq(beginning, middle, end).map(_.toString)
  }

  "The Transaction Rule Loader" should "load a single transaction" in {
    new Loader(compact(render(oneRuleJson))).parse shouldEqual
      Right(oneRuleExpected)
  }

  it should "handle comments" in {
    oneRuleCommented.map(new Loader(_).parse) shouldEqual (0 to 3).map(_ => Right(oneRuleExpected))
  }
}
