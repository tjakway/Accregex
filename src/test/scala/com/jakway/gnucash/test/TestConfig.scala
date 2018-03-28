package com.jakway.gnucash.test

import com.jakway.gnucash.parser.test.TestParser
import com.jakway.gnucash.rules.test.TestLinkAccounts

/**
  * Test configuration file containing test resources and instantiations
  * of test classes with those resources
  */

object TestResources {
  val regDocXML = "/reg_doc_example.gnucash"
}
import TestResources._

class TestLinkAccountsRegDocXML extends TestLinkAccounts(regDocXML)

class TestParserRegDocXML extends TestParser(regDocXML)
