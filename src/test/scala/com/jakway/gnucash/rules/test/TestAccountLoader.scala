package com.jakway.gnucash.rules.test

import com.jakway.gnucash.parser.{Parser, UnlinkedAccount, ValidationError}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node

class TestAccountLoader extends FlatSpec with Matchers {
  "The Account Loader" should "disambiguate a top-level account" in {

  }


}

class TestLinkAccounts(val regDocRoot: Node) extends FlatSpec with Matchers {

  /**
    * for errors thrown during test setup
    * @param msg
    */
  case class TestLinkAccountsLoadError(override val msg: String)
    extends ValidationError(msg)

  lazy val unlinkedAccounts: Map[String, UnlinkedAccount] =
    new Parser()
      .parseAccountNodes(regDocRoot)
      .map(accs => accs.map(a => (a.id, a)))
      .map(_.toMap)
      .getOrElse(throw TestLinkAccountsLoadError("Error while loading " +
          "TestLinkAccounts.unlinkedAccounts"))

  lazy val linkedAccounts = Parser
    .linkAccounts(unlinkedAccounts.values.toSeq)
    .map(_.map(a => (a.id, a)))
    .map(_.toMap)
    .getOrElse(throw TestLinkAccountsLoadError("Error while loading " +
      "TestLinkAccounts.linkedAccounts"))

  object TestObjects {
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
  import TestObjects._

  /**
    * tests of the unlinkedAccounts test object
    */

  //make sure the regDocRoot param has unlinked accounts we can parse successfully
  "linkAccounts.unlinkedAccounts" should "have valid unlinked accounts" in {
    //force the lazy value and make sure it isn't empty
    unlinkedAccounts.isEmpty shouldEqual false
  }

  it should "have an assets account and a current assets account" in {
    unlinkedAccounts.get(assetAccountId) shouldEqual Some(Unlinked.assetAccount)
    unlinkedAccounts.get(currentAssetsAccountId) shouldEqual Some(Unlinked.currentAssetsAccount)
  }

  it should "contain the expected unlinked accounts" in {
    unlinkedAccounts.values.toSet.intersect(
      Unlinked.unlinkedAccountsTestObjects.toSet) shouldEqual Unlinked.unlinkedAccountsTestObjects.toSet
  }

  it should "contain the root account" in {
    unlinkedAccounts.get(rootAccountId) shouldEqual Some(Unlinked.rootAccount)
  }

  //test parentId 2 levels down
  it should "correctly nest currentAssets parentId" in {
    unlinkedAccounts.get(currentAssetsAccountId).flatMap(_.parentId) shouldEqual Some(assetAccountId)
  }

  //test parentId 1 level down
  it should "correctly nest Assets parent id" in {
    unlinkedAccounts.get(assetAccountId).flatMap(_.parentId) shouldEqual Some(rootAccountId)
  }

  //root account shouldn't have a parentId
  it should "correctly handle the root account's parentId" in {
    unlinkedAccounts.get(rootAccountId).flatMap(_.parentId) shouldEqual None
  }

  /**
    * Parser.linkAccounts tests
    */


  "linkAccounts" should "link the root account" in {
    linkedAccounts.get(rootAccountId) shouldEqual Some(Linked.rootAccount)
  }

  "linkAccounts" should "link the asset account" in {
    linkedAccounts.get(assetAccountId) shouldEqual Some(Linked.assetAccount)
  }

  "linkAccounts" should "link the current assets account" in {
    linkedAccounts.get(currentAssetsAccountId) shouldEqual Some(Linked.currentAssetsAccount)
  }

  "linkAccounts" should "construct a walkable tree from the current assets account" in {
     linkedAccounts.get(currentAssetsAccountId)
       .flatMap(_.parent.flatMap(_.parent)) shouldEqual Some(Linked.rootAccount)
  }



}


