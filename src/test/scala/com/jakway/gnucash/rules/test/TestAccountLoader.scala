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

  //make sure the regDocRoot param has unlinked accounts we can parse successfully
  "linkAccounts" should "be passed valid unlinked accounts" in {
    //force the lazy value and make sure it isn't empty
    unlinkedAccounts.isEmpty shouldEqual false
  }

  "linkAccounts" should "be passed an assets account and a current assets account" in {
    unlinkedAccounts.get(assetAccountId) shouldEqual Some(assetAccount)
    unlinkedAccounts.get(currentAssetsAccountId) shouldEqual Some(currentAssetsAccount)
  }

  "linkAccounts" should "contain the expected unlinked accounts" in {
    unlinkedAccounts.values.toSet.union(unlinkedAccountsTestObjects.toSet) shouldEqual unlinkedAccountsTestObjects.toSet
  }

  it should "link top-level accounts" in {

  }
}


