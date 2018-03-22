package com.jakway.gnucash.parser

import com.jakway.util.XMLUtils

import scala.util.Try
import scala.xml.Node

case class UnlinkedAccount(version: String,
                   id: String,
                   name: String,
                   accType: String,
                   description: Option[String],
                   parentId: Option[String])

class Parser {
  import NodeTests._

  private def findBookNode(root: Node): ValidateF[Node, Node] =
    (root, errorType: String => ValidationError) => {
      def isBookNode(n: Node): Boolean = {
        n.label == "book" &&
          getAttribute((n, "version"))(errorType).isRight &&
          hasNamespace((n, "gnc"))(errorType).isRight
      }

      findOnlyOne((isBookNode _, root))(errorType)
    }

  private def isCountDataNode(n: Node): Boolean = {
    //check if it's a number
    Try(n.text.toInt).isSuccess &&
      //and has the expected attribute and namespace
      getAttribute((n, "cd:type"))(_ => new ValidationError {}).isRight &&
      hasNamespace((n, "gnc"))(_ => new ValidationError {}).isRight
  }

  def extractNumAccounts(book: Node): Either[ValidationError, Int] = {
    case class ExtractNumAccountsError(msg: String) extends ValidationError
    implicit def errorType: String => ValidationError = ExtractNumAccountsError.apply


    //find the right node and extract the text
    val foundNode = onlyOne(XMLUtils.searchNode(isCountDataNode)(book)
      .filter(n => expectAttribute((n, "cd:type", "account"))(errorType).isRight))
      .flatMap(getNodeText.apply)

    //convert the text to an integer and return it
    foundNode match {
      case Right(node) => Try(node.toInt).toEither match {
        case Left(t) => Left(errorType(s"Could not convert text of node $book '$node' to an integer"))
        case Right(q) => Right(q)
      }

      case Left(q) => Left(q)
    }
  }

  def extractNumTransactions(book: Node): Either[ValidationError, Int] = {
    case class ExtractNumTransactionsError(msg: String) extends ValidationError
    implicit def errorType: String => ValidationError = ExtractNumTransactionsError.apply


    onlyOne(XMLUtils.searchNode(isCountDataNode)(book)
      .filter(n => getAttribute((n, "cd:type"))
        .filterOrElse(_ == "transaction", Left()).isRight))
  }

  def parseAccountNode(n: Node): Either[ValidationError, UnlinkedAccount] = {
    case class ParseAccountNodeError(msg: String) extends ValidationError
    implicit def errorType: String => ValidationError = ParseAccountNodeError.apply

    for {
      _ <- hasNamespace((n, "gnc"))
      version <- getAttribute((n, "version"))

      accountName <- getElem((n, "name")).flatMap(getNodeText.apply)

      //id node of type guid
      idNode <- getElem((n, "id"))
      _ <- expectAttribute((idNode, "type", "guid"))

      id <- getNodeText(idNode)

      accountType <- getElem((n, "type")).flatMap(getNodeText.apply)

    } yield {
      //retrieve the description if there is one
      val description = getElem((n, "description")).flatMap(getNodeText.apply).toOption

      //similarly get the parent (if there is one)
      val parentId = for {
        parent <- getElem((n, "parent"))
        _ <- expectAttribute((parent, "type", "guid"))
        parentNodeId <- getNodeText(parent)
      } yield {
        parentNodeId
      }


      UnlinkedAccount(version, id, accountName, accountType, description, parentId.toOption)
    }
  }

}
