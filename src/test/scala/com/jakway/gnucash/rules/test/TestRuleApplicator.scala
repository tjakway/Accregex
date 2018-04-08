package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser.rules.{Transaction, UnlinkedTransactionRule}
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

    val targetAccountString = "Expenses:Auto:Gas"
    val description = "Car Repair"
    val unlinkedChangeGasRule = UnlinkedTransactionRule(description,
      "1", "Assets:Current Assets:Checking Account", "Expenses:Charity")

    val nameParser = new AccountNameParser(allAccounts.values.toSeq)

    val applicatorE = for {
      //link the changeGasRule to the referenced account names
      changeGasRule <- UnlinkedTransactionRule
                      .link(nameParser)(unlinkedChangeGasRule)

      targetAccount <- nameParser.findReferencedAccount(targetAccountString)
    } yield {
      (targetAccount, new RuleApplicator(
        allAccounts, targetAccount, Set(changeGasRule)))
    }

    val (targetAccount, applicator) = applicatorE.right.get

    targetAccount shouldEqual testObjects.Linked.gasAccount

    val allTransactionNodes = {
      for {
        bookNode <- parser.findBookNode(regDocRoot)
        transactions <- Parser.getTransactionNodes((bookNode, allAccounts))
      } yield(transactions)
    }.right.get

    //clone the old nodes just in case
    val clonedNodes = allTransactionNodes.map(_.asInstanceOf[Elem].copy())
    val oldNodes = allTransactionNodes
    val newNodes = allTransactionNodes.map(applicator.doReplace(_)._2)


    def parseTrans = Parser.parseTransaction(allAccounts) _

    val t: Either[Seq[ValidationError], (Seq[Transaction], Seq[Transaction])] =
    for {
      newTransactions <- ValidationError accumulateEithersSimpleSeq newNodes.map(parseTrans)
      oldTransactions <- ValidationError accumulateEithersSimpleSeq oldNodes.map(parseTrans)
    } yield {
      (newTransactions, oldTransactions)
    }
    val (newTransactions, oldTransactions) = t.right.get

    clonedNodes == oldNodes
    newNodes.xml_!=(oldNodes) shouldEqual true
    newNodes.toString != oldNodes.toString shouldEqual true
    newTransactions != oldTransactions shouldEqual true

    //make sure the output nodes match what we expect
    val changedTransactions = newTransactions.filter(_.description == description)
    changedTransactions.length shouldEqual 1
    val changedSplits = changedTransactions.head.splits
    changedSplits.size shouldEqual 3

    changedSplits.map(_.on.name) shouldEqual Set("Checking Account",
      "Repair and Maintenance", "Charity")

    val prevTransaction = oldTransactions.filter(_.description == description)
    prevTransaction.length shouldEqual 1
    prevTransaction != changedTransactions shouldEqual true

    //make sure the values of the splits stay the same
    def getSplitValues(e: Seq[Transaction]) = e.head.splits.map(_.value)
    getSplitValues(prevTransaction) shouldEqual getSplitValues(changedTransactions)


  }

}
