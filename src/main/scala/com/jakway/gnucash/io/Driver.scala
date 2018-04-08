package com.jakway.gnucash.io

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.parser._
import com.jakway.gnucash.parser.rules.UnlinkedTransactionRule
import com.jakway.gnucash.rules.LinkedTransactionRule

import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node, XML}

object Driver {
  case class GnucashXMLLoadError(override val msg: String)
    extends ValidationError(msg)
}

class Driver(val config: ValidatedConfig) {
  import Driver._
  val parser = new Parser()

  def run(): Unit = ???

  def runEither(): Either[ValidationError, Unit] = {
    for {
      rootNode <- loadGnucashXMLFile()
      bookNode <- parser.findBookNode(rootNode)(GnucashXMLLoadError.apply(_))
      accounts <- loadAccounts(bookNode)
    } yield {}
  }

  private def loadAccounts(book: Node) = {
    parser
      .parseAccountNodes(book)
      .flatMap(Parser.linkAccounts)
  }

  def loadRules(accountNameParser: AccountNameParser)
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
      unlinkedRules: Seq[UnlinkedTransactionRule] <- MultiValidationError.
                                                      wrap(transactionRuleLoader.parse)

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
