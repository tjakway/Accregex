package com.jakway.gnucash.rules.test

import com.jakway.gnucash.Config
import com.jakway.gnucash.parser.ValidationError
import com.jakway.gnucash.parser.rules.UnlinkedTransactionRule
import com.jakway.gnucash.rules.Loader
import org.json4s.jackson.JsonMethods._
import org.scalatest.{FlatSpec, Matchers}

class TestRuleLoader(verbosity: Config.Verbosity) extends FlatSpec with Matchers {
  import org.json4s._

  implicit val formats = DefaultFormats

  object TestObjects {
    import org.json4s.JsonDSL._

    val oneRuleName = "My Rule"

    val oneRuleJson: JValue = (
      (oneRuleName ->
        ("pattern" -> ".*") ~
        ("priority" -> 1) ~
        ("sourceAccount" -> "bar") ~
        ("destAccount" -> "baz"))
      )

    val oneRuleExpected = UnlinkedTransactionRule(
      oneRuleName, ".*", 1.toDouble.toString, "bar", "baz")

    lazy val oneRuleCommented = insertComments("//examplecomment")(pretty(render(oneRuleJson)))


    val multipleRuleNames = Seq("bar", "baz")
    val multipleRules = (
      (multipleRuleNames(0) ->
        ("pattern" -> "\\s+") ~
        ("priority" -> 2433) ~
        ("sourceAccount" -> "sklje") ~
        ("destAccount" -> "qjeeew")) ~
      (multipleRuleNames(1) ->
        ("pattern" -> "\\d*") ~
        ("priority" -> 30) ~
        ("sourceAccount" -> "xxzl") ~
        ("destAccount" -> "jy422"))
      )

    val multipleRulesExpected = Seq(
        UnlinkedTransactionRule(multipleRuleNames(0), "\\s+", 2433.toDouble.toString, "sklje", "qjeeew"),
        UnlinkedTransactionRule(multipleRuleNames(1), "\\d*", 30.toDouble.toString, "xxzl", "jy422"))

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
    new Loader(verbosity, compact(render(oneRuleJson))).parse shouldEqual
      Right(Seq(oneRuleExpected))
  }

  it should "handle comments" in {
    val actual = ValidationError.accumulateEithers(oneRuleCommented.map(new Loader(verbosity, _).parse))
    actual shouldEqual Right((0 to 2).toList.map(_ => oneRuleExpected))
  }

  it should "handle comments in multiple rules" in {
    //need to sort them to compare them properly so put them in a set
    val actual = ValidationError.accumulateEithers(multipleRulesCommented.map(new Loader(verbosity, _).parse))
      .right.map(_.toSet)
    actual shouldEqual
      Right((0 to 2).toList.flatMap(_ => multipleRulesExpected).toSet)
  }
}
