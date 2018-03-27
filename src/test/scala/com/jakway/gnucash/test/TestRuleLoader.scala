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
  }
  import TestObjects._

  "The Transaction Rule Loader" should "load a single transaction" in {
    new Loader(compact(render(oneRuleJson))).parse shouldEqual
      Right(oneRuleExpected)
  }
}
