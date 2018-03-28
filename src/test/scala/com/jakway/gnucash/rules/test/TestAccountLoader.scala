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

  object TestObjects {
    val rootAccountId = "f52c28f32edd309e768494995470343b"
    val rootAccount = UnlinkedAccount("2.0.0",
      rootAccountId,
      "Root Account",
      "ROOT",
      None,
      None)

    val assetAccountId = "90313cf9a5c83a232cb040ba2fe315be"
    val assetAccount = UnlinkedAccount("2.0.0",
      assetAccountId,
      "Assets",
      "ASSET",
      Some("Assets"),
      Some(rootAccountId))

    val currentAssetsAccountId = "794a0c0fac9d75f85cf7c99141c9caac"
    val currentAssetsAccount = UnlinkedAccount("2.0.0",
      currentAssetsAccountId,
      "Current Assets",
      "ASSET",
      Some("Current Assets"),
      Some(assetAccountId))

    val unlinkedAccountsTestObjects = Seq(assetAccount, currentAssetsAccount)
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
    unlinkedAccounts.get(assetAccountId) shouldEqual Some(assetAccount)
    unlinkedAccounts.get(currentAssetsAccountId) shouldEqual Some(currentAssetsAccount)
  }

  it should "contain the expected unlinked accounts" in {
    unlinkedAccounts.values.toSet.intersect(unlinkedAccountsTestObjects.toSet) shouldEqual unlinkedAccountsTestObjects.toSet
  }

  it should "contain the root account" in {
    unlinkedAccounts.get(rootAccountId) shouldEqual Some(rootAccount)
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

  "linkAccounts" should "link top-level accounts" in {

  }
}


