package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser._
import com.jakway.gnucash.test.objects.RegDocTestObjects
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node



object TestLinkAccounts {
  /**
    * TODO: could reduce duplication by making a trait Account and having both
    * UnlinkedAccount and LinkedAccount inherit from it
    * @param u
    * @param l
    * @return
    */
  def cmpUnlinkedToLinked(u: UnlinkedAccount, l: LinkedAccount): Boolean = {
    val parentsMatch = u.parentId == l.parent.map(_.id)

    u.id == l.id &&
    u.accType == l.accType &&
    u.description == l.description &&
    parentsMatch &&
    u.version == l.version
  }
}



class TestLinkAccounts(val regDocRoot: Node) extends FlatSpec with Matchers {

  /**
    * for errors thrown during test setup
    * @param msg
    */
  case class TestLinkAccountsLoadError(override val msg: String)
    extends ValidationError(msg)

  val testObjects = new RegDocTestObjects(regDocRoot)

  /**
    * tests of the unlinkedAccounts test object
    */

  //make sure the regDocRoot param has unlinked accounts we can parse successfully
  "linkAccounts.unlinkedAccounts" should "have valid unlinked accounts" in {
    //force the lazy value and make sure it isn't empty
    testObjects.unlinkedAccounts.isEmpty shouldEqual false
  }

  it should "have an assets account and a current assets account" in {
    testObjects.unlinkedAccounts.get(testObjects.assetsAccountId) shouldEqual Some(testObjects.Unlinked.assetAccount)
    testObjects.unlinkedAccounts.get(testObjects.currentAssetsAccountId) shouldEqual Some(testObjects.Unlinked.currentAssetsAccount)
  }

  it should "contain the expected unlinked accounts" in {
    testObjects.unlinkedAccounts.values.toSet.intersect(
      testObjects.Unlinked.unlinkedAccountsTestObjects.toSet) shouldEqual testObjects.Unlinked.unlinkedAccountsTestObjects.toSet
  }

  it should "contain the root account" in {
    testObjects.unlinkedAccounts.get(testObjects.rootAccountId) shouldEqual Some(testObjects.Unlinked.rootAccount)
  }

  //test parentId 2 levels down
  it should "correctly nest currentAssets parentId" in {
    testObjects.unlinkedAccounts.get(testObjects.currentAssetsAccountId).flatMap(_.parentId) shouldEqual Some(testObjects.assetsAccountId)
  }

  //test parentId 1 level down
  it should "correctly nest Assets parent id" in {
    testObjects.unlinkedAccounts.get(testObjects.assetsAccountId).flatMap(_.parentId) shouldEqual Some(testObjects.rootAccountId)
  }

  //root account shouldn't have a parentId
  it should "correctly handle the root account's parentId" in {
    testObjects.unlinkedAccounts.get(testObjects.rootAccountId).flatMap(_.parentId) shouldEqual None
  }

  /**
    * Parser.linkAccounts tests
    */


  "linkAccounts" should "link the root account" in {
    testObjects.linkedAccounts.get(testObjects.rootAccountId) shouldEqual Some(testObjects.Linked.rootAccount)
  }

  it should "link the asset account" in {
    testObjects.linkedAccounts.get(testObjects.assetsAccountId) shouldEqual Some(testObjects.Linked.assetAccount)
  }

  it should "link the current assets account" in {
    testObjects.linkedAccounts.get(testObjects.currentAssetsAccountId) shouldEqual Some(testObjects.Linked.currentAssetsAccount)
  }

  it should "construct a walkable tree from the current assets account" in {
     testObjects.linkedAccounts.get(testObjects.currentAssetsAccountId)
       .flatMap(_.parent.flatMap(_.parent)) shouldEqual Some(testObjects.Linked.rootAccount)
  }

  it should "construct a walkable tree from the expenses account" in {
    testObjects.linkedAccounts
      .get(testObjects.expensesAccountId)
      .flatMap(_.parent) shouldEqual Some(testObjects.Linked.rootAccount)
  }

  it should "construct a walkable tree from the charity expenses account" in {
    val charityAccount = testObjects.linkedAccounts.get(testObjects.charityAccountId)

    TestLinkAccounts
      .cmpUnlinkedToLinked(testObjects.Unlinked.charityAccount,
        charityAccount.get) shouldEqual true

    val lookedUpExpenseAccount = charityAccount.flatMap(_.parent)
    lookedUpExpenseAccount shouldEqual Some(testObjects.Linked.expenseAccount)
    lookedUpExpenseAccount
      .flatMap(_.parent) shouldEqual Some(testObjects.Linked.rootAccount)
  }
}


class TestAccountNameParser(val regDocRoot: Node) extends FlatSpec with Matchers {
  case class TestAccountNameParserError(override val msg: String)
    extends ValidationError(msg)
  implicit def errorType: String => ValidationError = TestAccountNameParserError.apply

  lazy val testObjects = new RegDocTestObjects(regDocRoot)
  lazy val nameParser = new AccountNameParser(testObjects.linkedAccounts.values.toSeq)

  //the user should NOT be able to reference the root account by name because
  //it's an "ephemeral" account (not visible in the account tree in GNUCash)
  "AccountNameParser" should "not disambiguate a top-level account" in {
    nameParser
      .findReferencedAccount(testObjects.rootAccountName).isLeft shouldEqual true
  }

  def secondLevelTest(accountStr: String, expectedAccount: LinkedAccount)= {
    nameParser.findReferencedAccount(accountStr) shouldEqual
      Right(expectedAccount)
  }

  def thirdLevelTest(accountStr: String,
                     expectedAccount: LinkedAccount,
                     expectedNames: Seq[String]) = {
    nameParser.splitAccountStr(accountStr) shouldEqual expectedNames

    secondLevelTest(accountStr, expectedAccount)
  }

  it should "disambiguate a second-level asset account" in {
      secondLevelTest("Assets", testObjects.Linked.assetAccount)
  }

  it should "disambiguate a third-level asset account" in {
    thirdLevelTest("Assets:Current Assets",
      testObjects.Linked.currentAssetsAccount,
      Seq(
        testObjects.Linked.currentAssetsAccount.name,
        testObjects.Linked.assetAccount.name))
  }

  it should "disambiguate a second-level expense account" in {
    secondLevelTest("Expenses", testObjects.Linked.expenseAccount)
  }

  it should "disambiguate a third-level expense account" in {
    secondLevelTest("Expenses:Charity",
      testObjects.Linked.charityAccount)
  }
}
