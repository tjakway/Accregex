package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser.rules.UnlinkedTransactionRule
import com.jakway.gnucash.parser.xml.NodeTests
import com.jakway.gnucash.parser.{AccountNameParser, LinkedAccount, Parser, ValidationError}
import com.jakway.gnucash.rules.{LinkedTransactionRule, RuleApplicator}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node

class TestRuleApplicator(val regDocRoot: Node) extends FlatSpec with Matchers {
  val parser: Parser = new Parser()

  case class TestRuleApplicatorError(override val msg: String)
    extends ValidationError(msg)
  implicit def errorType: String => ValidationError = TestRuleApplicatorError.apply

  "RuleApplicator" should "change gas to charity" in {
    val allAccounts: Map[String, LinkedAccount] = Parser.linkAccounts(parser
      .parseAccountNodes(regDocRoot)
      .right
      .get)
      .right
      .get
      .map(l => (l.id, l))
      .toMap

    val gasAccountString = "Expense:Auto:Gas"
    val unlinkedChangeGasRule = UnlinkedTransactionRule(".*",
      "1", "Assets:Current Assets", "Expense:Charity")

    val nameParser = new AccountNameParser(allAccounts.values.toSeq)

    val applicatorE = for {
      //link the changeGasRule to the referenced account names
      changeGasRule <- UnlinkedTransactionRule
                      .link(nameParser)(unlinkedChangeGasRule)

      gasAccount <- nameParser.findReferencedAccount(gasAccountString)
    } yield {
      new RuleApplicator(
        allAccounts, gasAccount, Set(changeGasRule))
    }

    applicatorE.isRight shouldEqual true
    val applicator = applicatorE.right.get

    val allTransactionNodes = NodeTests.getElems((regDocRoot, "transaction"))
      .right.get

    val newNodes = allTransactionNodes.map(applicator.apply(_)._2)

    newNodes == allTransactionNodes shouldEqual false

  }

}
