package com.jakway.gnucash.parser.xml

import java.io.StringReader

import com.jakway.gnucash.parser.{Parser, ValidationError}
import com.jakway.gnucash.parser.rules.{Split, Transaction}
import com.jakway.gnucash.util.PrintNode
import org.w3c.dom.Node
import org.xmlunit.builder.DiffBuilder
import org.xmlunit.diff.{DefaultNodeMatcher, Difference, ElementSelectors}
import org.xmlunit.util.Predicate
import scala.collection.JavaConverters

trait BeforeAfterDiff {
  def passes(): Either[ValidationError, Unit]
}

object SetsEqual {
  def apply[A](cmp: (A, A) => Boolean)(left: Set[A], right: Set[A]): Boolean = {
    //two sets are equal if their lengths are the same and for every item in
    //set A there is a corresponding item in set B
    left.size == right.size &&
      left.forall(l => right.find(cmp(l, _)).isDefined)
  }
}

class XMLUnitDiff(val originalXML: String, val originalTransactions: Set[Transaction],
           val newXML: String, val newTransactions: Set[Transaction],
           val parseTransaction: scala.xml.Node => Either[ValidationError, Transaction])
  extends BeforeAfterDiff {

  class DiffError(override val msg: String) extends ValidationError(msg)

  case class TransactionsNotBijectiveError(override val msg: String)
    extends DiffError(msg)

  case class InvalidTransactionsError(override val msg: String)
    extends DiffError(msg)

  case class UnexpectedDifferencesError(val differences: Seq[Difference])
    extends DiffError(s"Unexpected differences in comparison between " +
      s"$originalXML and $newXML: $differences") {
    val diffOriginalXML = originalXML
    val diffNewXML = newXML
  }


  private def checkTransactionsAreBijective: Either[ValidationError, Unit] = {

    def cmpTransactionsIgnoreSplitAccounts(left: Transaction, right: Transaction): Boolean =
      left.description == right.description &&
        left.id == right.id &&
        SetsEqual(cmpSplitsIgnoreAccount)(left.splits, right.splits)


    def cmpSplitsIgnoreAccount(left: Split, right: Split): Boolean =
      left.value == right.value && left.id == right.id

    if(SetsEqual(cmpTransactionsIgnoreSplitAccounts)
             (originalTransactions,
               newTransactions)) {
      Right(())
    } else {
      Left(TransactionsNotBijectiveError(s"Expected ${originalTransactions} to match ${newTransactions}" +
        s" except for the destination account changes"))
    }
  }

  private def checkTransactionsValid(transactions: TraversableOnce[Transaction]): Either[ValidationError, Unit] =
    if(transactions.forall(_.isValid)) {
      Right(())
    } else {
      Left(InvalidTransactionsError(s"Invalid transactions passed to Diff: " +
        s"${transactions.filter(!_.isValid)}"))
    }

  /**
    * @return TODO: change to return a summary of the diffs
    */
  def check(): Either[ValidationError, Unit] = {
    for {
      _ <- checkTransactionsValid(originalTransactions)
      _ <- checkTransactionsValid(newTransactions)
      _ <- checkTransactionsAreBijective
    } yield {}
  }

  private lazy val diff = DiffBuilder.compare(originalXML)
    .withTest(newXML)
    .ignoreComments()
    .ignoreWhitespace()
    //ignore order of elements
    //see https://stackoverflow.com/questions/16540318/compare-two-xml-strings-ignoring-element-order?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
    .withNodeMatcher(new DefaultNodeMatcher(ElementSelectors.byNameAndText))
    .withNodeFilter(new ModifiedTransactionFilter(originalTransactions ++ newTransactions))
    .checkForSimilar()
    .build()


  val formattedDifferences: String = diff.toString()

  /**
    * exclude the transactions we just modified from the comparison
    * @param toIgnore
    */
  class ModifiedTransactionFilter(val toIgnore: Set[Transaction])
    extends Predicate[org.w3c.dom.Node] {

    def stringToXML(s: String): scala.xml.Node =
      scala.xml.XML.load(new StringReader(s))

    def xmlToString(node: org.w3c.dom.Node): String =
      PrintNode.printNode(node, true, true)

    //the node filter should return FALSE for elements that ought to be ignored
    override def test(toTest: org.w3c.dom.Node): Boolean = {
      //TODO: SUPER hacky way to implement this...
      //as is, we go from Java Node -> String -> Scala Node
      //then try to parse the scala node as a transaction
      //this is both slow and error prone
      //the solution will be to replace scala's XML library
      //with the native java one (javax.xml)
      val scalaNode = stringToXML(xmlToString(toTest))

      parseTransaction(scalaNode) match {
          //only ignore nodes that are part of our toIgnore set
        case Right(thisTrans)
          if toIgnore.contains(thisTrans) => false
        case _ => true
      }
    }
  }


  override def passes(): Either[ValidationError, Unit] = {
    if(diff.hasDifferences()) {
      val diffSeq = JavaConverters.iterableAsScalaIterable(diff.getDifferences()).toSeq
      Left(UnexpectedDifferencesError(diffSeq))
    } else {
      Right(())
    }
  }

}

class AlwaysPassesDiff extends BeforeAfterDiff {
  override def passes(): Either[ValidationError, Unit] = Right(())
}
