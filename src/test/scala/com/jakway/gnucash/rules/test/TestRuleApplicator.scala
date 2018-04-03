package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser.rules.UnlinkedTransactionRule
import com.jakway.gnucash.parser.{AccountNameParser, LinkedAccount, Parser}
import com.jakway.gnucash.rules.LinkedTransactionRule
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node

class TestRuleApplicator(val regDocRoot: Node) extends FlatSpec with Matchers {
  val parser: Parser = new Parser()

  "RuleApplicator" should "change gas to charity" in {
    val allAccounts: Map[String, LinkedAccount] = Parser.linkAccounts(parser
      .parseAccountNodes(regDocRoot)
      .right
      .get)
      .right
      .get
      .map(l => (l.id, l))
      .toMap

    val unlinkedChangeGasRule = UnlinkedTransactionRule(".*",
      "1", "Expense:Auto:Gas", "Expense:Charity")

    //link the changeGasRule to the referenced account names
    val nameParser = new AccountNameParser(allAccounts.values.toSeq)
    val changeGasRule = UnlinkedTransactionRule
      .link(nameParser)(unlinkedChangeGasRule)

    
  }

}
