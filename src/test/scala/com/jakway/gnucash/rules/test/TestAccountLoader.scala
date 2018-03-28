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
      .map(accs => accs.map(a => (a.name, a)))
      .map(_.toMap)
      .getOrElse(throw TestLinkAccountsLoadError("Error while loading " +
          "TestLinkAccounts.unlinkedAccounts"))

  //make sure the regDocRoot param has unlinked accounts we can parse successfully
  "linkAccounts" should "be passed valid unlinked accounts" in {
    //force the lazy value and make sure it isn't empty
    unlinkedAccounts.isEmpty shouldEqual false
  }

  "linkAccounts" should "link top-level accounts" in {

  }
}


