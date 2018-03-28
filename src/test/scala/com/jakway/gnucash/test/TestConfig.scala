package com.jakway.gnucash.test

import com.jakway.gnucash.parser.test.TestParser
import com.jakway.gnucash.rules.test.TestLinkAccounts

import scala.xml.XML

/**
  * Test configuration file containing test resources and instantiations
  * of test classes with those resources
  */

object TestResources {
  val regDocXML = "/reg_doc_example.gnucash"

  lazy val regDocRoot = XML.load(getClass().getResource(regDocXML))
}
import TestResources._

class TestLinkAccountsRegDocXML extends TestLinkAccounts(regDocRoot)

class TestParserRegDocXML extends TestParser(regDocRoot)
