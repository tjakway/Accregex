package com.jakway.gnucash.test

import com.jakway.gnucash.Config
import com.jakway.gnucash.io.test.TestXMLValidator
import com.jakway.gnucash.parser.test.{TestFilterTransactionsDiff, TestParser, TestXMLEqualDiff}
import com.jakway.gnucash.rules.test.{TestAccountNameParser, TestLinkAccounts, TestRuleApplicator, TestRuleLoader}

import scala.xml.XML

/**
  * Test configuration file containing test resources and instantiations
  * of test classes with those resources
  * TODO: merge with trait ResourceFiles
  */
object TestResources {
  val regDocXML = "/reg_doc_example.gnucash"
  val currencyTreeXML = "/currency_tree_xml.gnucash"

  lazy val regDocRoot = XML.load(getClass().getResource(regDocXML))
  lazy val currencyTreeRoot = XML.load(getClass().getResource(currencyTreeXML))

  val testConfigVerbosity: Config.Verbosity = Config.Verbosity(false, false, false)
}
import TestResources._

//test class instantiations
class TestRuleLoaderInst extends TestRuleLoader(testConfigVerbosity)

//XML test class instantiations
class TestLinkAccountsRegDocXML extends TestLinkAccounts(regDocRoot)

class TestParserRegDocXML extends TestParser(regDocRoot)

class TestAccountNameParserRegDocXML extends TestAccountNameParser(regDocRoot)

class TestRuleApplicatorRegDocXML extends TestRuleApplicator(regDocRoot)

class TestNodeReplaceRegDocXML extends TestNodeReplace(regDocRoot)

class TestXMLValidatorRegDocXML
  extends TestXMLValidator(regDocRoot, currencyTreeRoot)

class TestFilterTransactionsDiffRegDocXML
  extends TestFilterTransactionsDiff(regDocRoot)

class TestXMLEqualDiffRegDocXML
  extends TestXMLEqualDiff(regDocRoot)