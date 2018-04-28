package com.jakway.gnucash.io

import java.io._

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.parser._
import com.jakway.gnucash.parser.rules.{Transaction, UnlinkedTransactionRule}
import com.jakway.gnucash.parser.xml.{AlwaysPassesDiff, FilterTransactionsDiff, NodeTests}
import com.jakway.gnucash.rules.RuleApplicator.RuleApplicatorLogEvent
import com.jakway.gnucash.rules.{LinkedTransactionRule, RuleApplicator}
import com.jakway.util.XMLUtils

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

  case class WriteError(override val msg: String)
    extends ValidationError(msg)
}

class Driver(val config: ValidatedConfig) {
  import Driver._
  val parser = new Parser()

  def run(): Unit = {
    {
      for {
        output <- runEither()
        _ <- write(output)
      } yield {
        output
      }
    }
    match {
      case Right(o) => {
        if(config.verbosity.printSummary) {
          new RuleApplicatorEventPrinter(config.verbosity, o.events.toSet)
            .print()

          //we're done
          System.exit(0)
        }
      }

      case Left(err) => {
        System.err.println(ErrorPrinter.format(err))
        System.exit(1)
      }
    }
  }



  class Output(val compressionHandler: CompressionHandler,
               val node: scala.xml.Node,
               val events: Seq[RuleApplicatorLogEvent])


  def write(o: Output): Either[ValidationError, File] = {
    def wrap[A](t: Try[A], onError: String): Either[ValidationError, A] = t match {
      case Success(r) => Right(r)
      case Failure(t) => Left(WriteError(onError).withCause(t))
    }

    for {
      fos <- wrap(Try { new FileOutputStream(config.outputPath) },
        s"Error opening ${config.outputPath}")
      os <- o.compressionHandler.wrapOutputStream(fos)
      osw <- wrap(Try { new OutputStreamWriter(os, config.enc) },
        s"Error opening OutputStreamWriter around ${config.outputPath}")
      _ <- wrap(Try { XML.write(osw, o.node, config.enc, true, null) },
        s"Error while writing XML node to OutputStreamWriter")
    } yield {
      config.outputPath
    }
  }

  def runEither(): Either[ValidationError, Output] = {
    case class OrigEqualsOutputError(override val msg: String)
      extends ValidationError(msg)

    for {
      //decompress the input stream if it's gzipped
      //otherwise return it unchanged
      compressionHandler <- CompressionHandler.newGZIPHandler(config)
      xmlInputStream <- compressionHandler.inputToStream()

      (inputValidator, outputValidator) = XMLValidator.getValidators(config)
      //optionally validate the input file against the schema first
      //(note: XMLValidator will write the decompressed XML out to a temporary file)
      _ <- inputValidator.validate(config.inputPath.getName(), xmlInputStream)

      //parse the input stream as XML
      rootNode <- loadGnucashXML(xmlInputStream)
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

      //apply the rules and separate the results and log events
      ruleApplicatorOut <- Right(allTransactionNodes.map(ruleApplicator.doReplace(_)))
      outputTransactionNodes = ruleApplicatorOut.map(_._2)
      logEvents = ruleApplicatorOut.flatMap(_._1)

      newBookNode <- Parser.replaceTransactionNodes(accountMap)(bookNode, outputTransactionNodes)
      newRootNode <- Parser.replaceBookNode(rootNode)(newBookNode)

      _ <- NodeTests.checkNodesNotEqual((rootNode, newRootNode))(OrigEqualsOutputError.apply _)

      //check that the output matches what we expected based on our transformations
      _ <- checkDiff(rootNode, allTransactionNodes,
        newRootNode, outputTransactionNodes,
        Parser.parseTransaction(accountMap))

      //optionally validate the transformed XML document
      _ <- outputValidator.validateNode(s"output XML to be written to ${config.outputPath}",
        newRootNode)
    } yield {
      new Output(compressionHandler, newRootNode, logEvents.map(_._1))
    }
  }

  private def checkDiff(originalXML: Node, originalTransactions: Seq[Node],
           newXML: Node, newTransactions: Seq[Node],
           parseTransaction: scala.xml.Node => Either[ValidationError, Transaction]):
    Either[ValidationError, Unit] = {
    if(config.checkDiff) {
      for {
        //convert the scala xml nodes to strings so we can run XMLUnit on them
        originalXMLString <- XMLUtils.nodeToString(originalXML)
        newXMLString <- XMLUtils.nodeToString(newXML)

        //parse the transaction nodes
        //TODO: reuse the internal variables of Parser.replaceTransactionNodes/RuleApplicator instead
        //of redoing all parsing
        originalTransactions <- ValidationError.accumulateAndWrap(
          originalTransactions.map(parseTransaction))
        newTransactions <- ValidationError.accumulateAndWrap(
          newTransactions.map(parseTransaction))

        _ <- new FilterTransactionsDiff(originalXMLString, originalTransactions.toSet,
                newXMLString, newTransactions.toSet,
                parseTransaction).passes()
      } yield {}
    } else {
      new AlwaysPassesDiff().passes()
    }
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
      transactionRuleLoader = new com.jakway.gnucash.rules.Loader(config.verbosity, rawString)
      unlinkedRules <- transactionRuleLoader.parse

      linkedRules <- ValidationError.accumulateAndWrap(
        unlinkedRules.map(UnlinkedTransactionRule.link(accountNameParser)))
    } yield {
      linkedRules
    }
  }

  //TODO: handle (gzip) compressed gnucash input files
  private def loadGnucashXML(is: InputStream): Either[ValidationError, Elem] =
    Try(XML.load(is)) match {
      case Success(x) => Right(x)
      case Failure(t) => Left(new GnucashXMLLoadError(s"Error while loading GNUCash input stream" +
        s" (XML.load threw an exception)")
        .withCause(t))
    }
}
