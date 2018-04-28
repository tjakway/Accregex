package com.jakway.gnucash.test

import com.jakway.gnucash.Config
import com.jakway.gnucash.io.test.TestXMLValidator
import com.jakway.gnucash.parser.test.{TestFilterTransactionsDiff, TestParser, TestXMLEqualDiff}
import com.jakway.gnucash.rules.test.{TestAccountNameParser, TestLinkAccounts, TestRuleApplicator, TestRuleLoader}

import scala.xml.XML

trait TestConfig {
  val quiet: Config.Verbosity = Config.Verbosity(false, false, false)
}

object TestConfig extends TestConfig

/**
  * Test configuration file containing test resources and instantiations
  * of test classes with those resources
  * TODO: merge with trait ResourceFiles
  */
object TestResources extends ResourceNames {
  lazy val regDocRoot = XML.load(getClass().getResource(regDocXML))
  lazy val currencyTreeRoot = XML.load(getClass().getResource(currencyTreeXML))

}
import TestResources._

//test class instantiations
class TestRuleLoaderInst extends TestRuleLoader(TestConfig.quiet)

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