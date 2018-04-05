package com.jakway.gnucash.test

import com.jakway.gnucash.parser.test.TestParser
import com.jakway.gnucash.rules.test.{TestAccountNameParser, TestLinkAccounts, TestRuleApplicator}

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


//test class instantiations
class TestLinkAccountsRegDocXML extends TestLinkAccounts(regDocRoot)

class TestParserRegDocXML extends TestParser(regDocRoot)

class TestAccountNameParserRegDocXML extends TestAccountNameParser(regDocRoot)

class TestRuleApplicatorRegDocXML extends TestRuleApplicator(regDocRoot)

class TestNodeReplaceRegDocXML extends TestNodeReplace(regDocRoot)