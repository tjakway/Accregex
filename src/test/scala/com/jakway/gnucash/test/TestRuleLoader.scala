package com.jakway.gnucash.test

import com.jakway.gnucash.parser.ValidationError
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

    val oneRuleExpected = UnlinkedTransactionRule(
      ".*", 1.toDouble.toString, "bar", "baz")

    lazy val oneRuleCommented = insertComments("//examplecomment")(pretty(render(oneRuleJson)))


    val multipleRules = (Loader.jsonRoot ->
      ("qjdfss" ->
        ("pattern" -> "\\s+") ~
        ("priority" -> 2433) ~
        ("sourceAccount" -> "sklje") ~
        ("destAccount" -> "qjeeew")) ~
      ("wes" ->
        ("pattern" -> "\\d*") ~
        ("priority" -> 30) ~
        ("sourceAccount" -> "xxzl") ~
        ("destAccount" -> "jy422"))
      )

    val multipleRulesExpected = Seq(
      UnlinkedTransactionRule("\\s+", 2433.toDouble.toString, "sklje", "qjeeew"),
      UnlinkedTransactionRule("\\d*", 30.toDouble.toString, "xxzl", "jy422"))

    lazy val multipleRulesCommented = {
      val randomStr: String = (0 to Math.random().floor.toInt).map(_ => Math.random()).toString()
      insertComments("//" + randomStr)(pretty(render(multipleRules)))
    }
  }
  import TestObjects._

  def insertComments(comment: String)(js: String): Seq[String] = {

    //insert a comment in the middle of the Json
    val as = js.lines.take(js.lines.length / 2)
    val bs = js.lines.drop(js.lines.length / 2)
    assert(as != bs)

    val middle = as ++ Seq(comment) ++ bs

    assert(middle != js.lines)

    val beginning = Seq(comment) ++ js.lines
    val end = js.lines ++ Seq(comment)

    Seq(beginning, middle, end.toSeq).map(_.reduce(_ ++ "\n" ++ _))
  }

  "The Transaction Rule Loader" should "load a single transaction" in {
    new Loader(compact(render(oneRuleJson))).parse shouldEqual
      Right(Seq(oneRuleExpected))
  }

  it should "handle comments" in {
    val actual = ValidationError.accumulateEithers(oneRuleCommented.map(new Loader(_).parse))
    actual shouldEqual Right((0 to 2).toList.map(_ => oneRuleExpected))
  }

  it should "handle comments in multiple rules" in {
    //need to sort them to compare them properly so put them in a set
    val actual = ValidationError.accumulateEithers(multipleRulesCommented.map(new Loader(_).parse))
      .right.map(_.toSet)
    actual shouldEqual
      Right((0 to 2).toList.flatMap(_ => multipleRulesExpected).toSet)
  }
}
