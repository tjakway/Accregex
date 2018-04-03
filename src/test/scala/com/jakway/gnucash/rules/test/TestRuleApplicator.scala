package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser.rules.UnlinkedTransactionRule
import com.jakway.gnucash.parser.xml.NodeTests
import com.jakway.gnucash.parser.{AccountNameParser, LinkedAccount, Parser, ValidationError}
import com.jakway.gnucash.rules.{LinkedTransactionRule, RuleApplicator}
import com.jakway.gnucash.test.objects.RegDocTestObjects
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Elem, Node}

class TestRuleApplicator(val regDocRoot: Node) extends FlatSpec with Matchers {
  val parser: Parser = new Parser()
  lazy val testObjects: RegDocTestObjects = new RegDocTestObjects(regDocRoot)

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

    val gasAccountString = "Expenses:Auto:Gas"
    val unlinkedChangeGasRule = UnlinkedTransactionRule(".*",
      "1", "Assets:Current Assets", "Expenses:Charity")

    val nameParser = new AccountNameParser(allAccounts.values.toSeq)

    val applicatorE = for {
      //link the changeGasRule to the referenced account names
      changeGasRule <- UnlinkedTransactionRule
                      .link(nameParser)(unlinkedChangeGasRule)

      gasAccount <- nameParser.findReferencedAccount(gasAccountString)
    } yield {
      (gasAccount, new RuleApplicator(
        allAccounts, gasAccount, Set(changeGasRule)))
    }

    val (gasAccount, applicator) = applicatorE.right.get

    gasAccount shouldEqual testObjects.Linked.gasAccount

    val allTransactionNodes = NodeTests.getElems((regDocRoot, "transaction"))
      .right.get

    //clone the old nodes just in case
    val oldNodes = allTransactionNodes.map(_.asInstanceOf[Elem].copy())
    val newNodes = allTransactionNodes.map(applicator.doReplace(_)._2)

    def parseTrans = Parser.parseTransaction(allAccounts) _

    val (newTransactions, oldTransactions) = {for {
      newTransactions <- ValidationError accumulateEithersSimpleSeq newNodes.map(parseTrans)
      oldTransactions <- ValidationError accumulateEithersSimpleSeq oldNodes.map(parseTrans)
    } yield {
      (newTransactions, oldTransactions)
    }}.right.get



    newNodes.toString != oldNodes.toString shouldEqual true
    newTransactions != oldTransactions shouldEqual true

  }

}
