package com.jakway.gnucash.test.objects

import com.jakway.gnucash.parser.{Parser, UnlinkedAccount, ValidationError}

import scala.xml.Node

class RegDocTestObjects(val regDocRoot: Node) {
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

  val expensesAccountId = "b49c132fd93c94675a901feb8c5df41a"
  val expensesAccountName = "Expenses"

  val charityAccountId = "845aea6cf3402b138be1bcb6bbd62bc7"
  val charityAccountName = "Charity"

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

    val expensesAccount = UnlinkedAccount("2.0.0",
      expensesAccountId,
      expensesAccountName,
      "EXPENSE",
      Some("Expenses"),
      Some(rootAccountId))

    val charityAccount = UnlinkedAccount("2.0.0",
      charityAccountId,
      charityAccountName,
      "EXPENSE",
      Some("Charity"),
      Some(expensesAccountId))

    val unlinkedAccountsTestObjects = Seq(rootAccount, assetAccount, currentAssetsAccount,
      expensesAccount, charityAccount)
  }

  object Linked {
    val rootAccount = Unlinked.rootAccount.link(None)
    val assetAccount = Unlinked.assetAccount.link(Some(rootAccount))
    val currentAssetsAccount = Unlinked.currentAssetsAccount.link(Some(assetAccount))
    val expenseAccount = Unlinked.expensesAccount.link(Some(rootAccount))
    val charityAccount = Unlinked.charityAccount.link(Some(expenseAccount))

    val linkedAccountsTestObjects = Seq(rootAccount,
      assetAccount,
      currentAssetsAccount,
      expenseAccount,
      charityAccount)
  }
}
