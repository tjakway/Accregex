package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser.{AccountNameParser, Parser, UnlinkedAccount, ValidationError}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node

class TestObjects(val regDocRoot: Node) {
  case class TestObjectsException(override val msg: String)
    extends ValidationError(msg)

  lazy val unlinkedAccounts: Map[String, UnlinkedAccount] =
    new Parser()
      .parseAccountNodes(regDocRoot)
      .map(accs => accs.map(a => (a.id, a)))
      .map(_.toMap)
      .getOrElse(throw TestObjectsException("Error while loading " +
        "TestLinkAccounts.unlinkedAccounts"))

  lazy val linkedAccounts = Parser
    .linkAccounts(unlinkedAccounts.values.toSeq)
    .map(_.map(a => (a.id, a)))
    .map(_.toMap)
    .getOrElse(throw TestObjectsException("Error while loading " +
      "TestLinkAccounts.linkedAccounts"))

  val rootAccountId = "f52c28f32edd309e768494995470343b"
  val rootAccountName = "Root Account"

  val assetsAccountId = "90313cf9a5c83a232cb040ba2fe315be"
  val assetsAccountName = "Assets"

  val currentAssetsAccountId = "794a0c0fac9d75f85cf7c99141c9caac"
  val currentAssetsName = "Current Assets"

  object Unlinked {
    val rootAccount = UnlinkedAccount("2.0.0",
      rootAccountId,
      rootAccountName,
      "ROOT",
      None,
      None)

    val assetAccount = UnlinkedAccount("2.0.0",
      assetsAccountId,
      assetsAccountName,
      "ASSET",
      Some("Assets"),
      Some(rootAccountId))

    val currentAssetsAccount = UnlinkedAccount("2.0.0",
      currentAssetsAccountId,
      currentAssetsName,
      "ASSET",
      Some("Current Assets"),
      Some(assetsAccountId))

    val unlinkedAccountsTestObjects = Seq(rootAccount, assetAccount, currentAssetsAccount)
  }

  object Linked {
    val rootAccount = Unlinked.rootAccount.link(None)
    val assetAccount = Unlinked.assetAccount.link(Some(rootAccount))
    val currentAssetsAccount = Unlinked.currentAssetsAccount.link(Some(assetAccount))

    val linkedAccountsTestObjects = Seq(rootAccount, assetAccount, currentAssetsAccount)
  }
}



class TestLinkAccounts(val regDocRoot: Node) extends FlatSpec with Matchers {

  /**
    * for errors thrown during test setup
    * @param msg
    */
  case class TestLinkAccountsLoadError(override val msg: String)
    extends ValidationError(msg)

  val testObjects = new TestObjects(regDocRoot)

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
}


class TestAccountNameParser(val regDocRoot: Node) extends FlatSpec with Matchers {
  case class TestAccountNameParserError(override val msg: String)
    extends ValidationError(msg)
  implicit def errorType: String => ValidationError = TestAccountNameParserError.apply

  lazy val testObjects = new TestObjects(regDocRoot)
  lazy val nameParser = new AccountNameParser(testObjects.linkedAccounts.values.toSeq)

  //the user should NOT be able to reference the root account by name because
  //it's an "ephemeral" account (not visible in the account tree in GNUCash)
  "AccountNameParser" should "not disambiguate a top-level account" in {
    nameParser
      .findReferencedAccount(testObjects.rootAccountName).isLeft shouldEqual true
  }

  it should "disambiguate a second-level account" in {
    val str = "Assets"

    nameParser.findReferencedAccount(str) shouldEqual Right(testObjects.Linked.assetAccount)
  }

  it should "disambiguate a third-level account" in {
    val str = "Assets:Current Assets"

    nameParser.splitAccountStr(str) shouldEqual Seq(testObjects.Linked.assetAccount.name,
      testObjects.Linked.currentAssetsAccount.name)

    nameParser.findReferencedAccount(str) shouldEqual Right(testObjects.Linked.currentAssetsAccount)
  }
}
