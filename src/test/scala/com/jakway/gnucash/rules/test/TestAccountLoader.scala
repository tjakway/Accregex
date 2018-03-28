package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser.{Parser, UnlinkedAccount, ValidationError}
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

  val assetAccountId = "90313cf9a5c83a232cb040ba2fe315be"

  val currentAssetsAccountId = "794a0c0fac9d75f85cf7c99141c9caac"

  object Unlinked {
    val rootAccount = UnlinkedAccount("2.0.0",
      rootAccountId,
      "Root Account",
      "ROOT",
      None,
      None)

    val assetAccount = UnlinkedAccount("2.0.0",
      assetAccountId,
      "Assets",
      "ASSET",
      Some("Assets"),
      Some(rootAccountId))

    val currentAssetsAccount = UnlinkedAccount("2.0.0",
      currentAssetsAccountId,
      "Current Assets",
      "ASSET",
      Some("Current Assets"),
      Some(assetAccountId))

    val unlinkedAccountsTestObjects = Seq(rootAccount, assetAccount, currentAssetsAccount)
  }

  object Linked {
    val rootAccount = Unlinked.rootAccount.link(None)
    val assetAccount = Unlinked.assetAccount.link(Some(rootAccount))
    val currentAssetsAccount = Unlinked.currentAssetsAccount.link(Some(assetAccount))

    val linkedAccountsTestObjects = Seq(rootAccount, assetAccount, currentAssetsAccount)
  }
}


class TestAccountNameParser extends FlatSpec with Matchers {
  "AccountNameParser" should "disambiguate a top-level account" in {

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
    testObjects.unlinkedAccounts.get(testObjects.assetAccountId) shouldEqual Some(testObjects.Unlinked.assetAccount)
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
    testObjects.unlinkedAccounts.get(testObjects.currentAssetsAccountId).flatMap(_.parentId) shouldEqual Some(testObjects.assetAccountId)
  }

  //test parentId 1 level down
  it should "correctly nest Assets parent id" in {
    testObjects.unlinkedAccounts.get(testObjects.assetAccountId).flatMap(_.parentId) shouldEqual Some(testObjects.rootAccountId)
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

  "linkAccounts" should "link the asset account" in {
    testObjects.linkedAccounts.get(testObjects.assetAccountId) shouldEqual Some(testObjects.Linked.assetAccount)
  }

  "linkAccounts" should "link the current assets account" in {
    testObjects.linkedAccounts.get(testObjects.currentAssetsAccountId) shouldEqual Some(testObjects.Linked.currentAssetsAccount)
  }

  "linkAccounts" should "construct a walkable tree from the current assets account" in {
     testObjects.linkedAccounts.get(testObjects.currentAssetsAccountId)
       .flatMap(_.parent.flatMap(_.parent)) shouldEqual Some(testObjects.Linked.rootAccount)
  }
}


