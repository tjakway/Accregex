package com.jakway.gnucash.io

import java.io.PrintWriter

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.parser._
import com.jakway.gnucash.parser.rules.UnlinkedTransactionRule
import com.jakway.gnucash.rules.{LinkedTransactionRule, RuleApplicator}

import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node, XML}

object Driver {
  class GnucashXMLLoadError(override val msg: String)
    extends ValidationError(msg)

  object GnucashXMLLoadError {
    def apply: String => GnucashXMLLoadError = new GnucashXMLLoadError(_)
  }

  class LoadTransactionNodesError(override val msg: String)
    extends GnucashXMLLoadError(msg)

  object LoadTransactionNodesError {
    def apply: String => LoadTransactionNodesError = new LoadTransactionNodesError(_)
  }
}

class Driver(val config: ValidatedConfig) {
  import Driver._
  val parser = new Parser()

  //schema validation can be disabled via flags
  lazy val inputValidator: XMLValidator =
    if(config.skipInputValidation) {
      new GnucashXMLValidator()
    } else {
      new SkipXMLValidator()
    }

  lazy val outputValidator: XMLValidator =
    if(config.skipOutputValidation) {
      new GnucashXMLValidator()
    } else {
      new SkipXMLValidator()
    }


  def run(): Unit = runEither() match {
    case Right(newXML) => {
      XML.write(new PrintWriter(config.outputPath), newXML, config.enc,
        true, null) //null means no doctyp
    }
    case Left(err) => {
      System.err.println(ErrorPrinter.format(err))
      System.exit(1)
    }
  }

  def runEither(): Either[ValidationError, Node] = {
    for {
      //load the input XML file
      rootNode <- loadGnucashXMLFile()
      bookNode <- parser.findBookNode(rootNode)(GnucashXMLLoadError.apply(_))

      //parse & extract the accounts
      accounts <- loadAccounts(bookNode)
      accountNameParser = new AccountNameParser(accounts)

      targetAccount <- getTargetAccount(accountNameParser)
      rules <- loadRules(accountNameParser)(accounts)

      accountMap = accounts.map(x => (x.id, x)).toMap
      ruleApplicator = new RuleApplicator(accountMap, targetAccount, rules.toSet)

      allTransactionNodes <- Parser
        .getTransactionNodes((bookNode, accountMap))(LoadTransactionNodesError.apply)

      //TODO: handle tags outputted by RuleApplicator.doReplace
      outputTransactionNodes = allTransactionNodes.map(ruleApplicator.doReplace(_)._2)

      newBookNode <- Parser.replaceTransactionNodes(accountMap)(bookNode, outputTransactionNodes)
      newRootNode <- Parser.replaceBookNode(rootNode)(newBookNode)
    } yield (newRootNode)
  }

  private def loadAccounts(book: Node) = {
    parser
      .parseAccountNodes(book)
      .flatMap(Parser.linkAccounts)
  }

  private def getTargetAccount(accountNameParser: AccountNameParser):
    Either[ValidationError, LinkedAccount] =
    accountNameParser.findReferencedAccount(config.targetAccount)

  private def loadRules(accountNameParser: AccountNameParser)
               (accounts: Seq[LinkedAccount]):
    Either[ValidationError, Seq[LinkedTransactionRule]] = {

    def read() = {
      case class LoadRulesError(override val msg: String)
        extends ValidationError(msg)

      Try(Source.fromFile(config.rulesPath, config.enc).mkString) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(
          LoadRulesError(s"Could not read file ${config.rulesPath}")
          .withCause(t))
      }
    }

    for {
      rawString <- read()
      transactionRuleLoader = new com.jakway.gnucash.rules.Loader(rawString)
      unlinkedRules <- MultiValidationError.wrap(transactionRuleLoader.parse)

      linkedRules <- ValidationError.accumulateAndWrap(
        unlinkedRules.map(UnlinkedTransactionRule.link(accountNameParser)))
    } yield {
      linkedRules
    }
  }

  //TODO: handle (gzip) compressed gnucash input files
  private def loadGnucashXMLFile(): Either[ValidationError, Elem] =
    Try(XML.loadFile(config.inputPath)) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(new GnucashXMLLoadError(s"Error while loading GNUCash input file" +
        s" ${config.inputPath} (XML.loadFile threw an exception)")
        .withCause(t))
    }
}
