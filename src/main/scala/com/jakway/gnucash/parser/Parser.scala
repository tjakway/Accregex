package com.jakway.gnucash.parser

import com.jakway.util.XMLUtils

import scala.util.Try
import scala.xml.Node

case class Account(version: Option[String],
                   id: String,
                   name: String,
                   accType: String,
                   parent: Option[Account])

class Parser {
  import NodeTests._

  private def findBookNode(root: Node): ValidateF[Node, Node] =
    (root, errorType: String => ValidationError) => {
      def isBookNode(n: Node): Boolean = {
        n.label == "book" &&
          getAttribute((n, "version")).isRight &&
          hasNamespace((n, "gnc")).isRight
      }

      findOnlyOne((isBookNode _, root))
    }

  private def isCountDataNode(n: Node): Boolean = {
    //check if it's a number
    Try(n.text.toInt).isSuccess &&
      //and has the expected attribute and namespace
      getAttribute((n, "cd:type")).isRight &&
      hasNamespace((n, "gnc")).isRight
  }

  def extractNumAccounts(book: Node): Either[ValidationError, Int] = {
    case class ExtractNumAccountsError(msg: String) extends ValidationError
    implicit def errorType: String => ValidationError = ExtractNumAccountsError.apply


    onlyOne(XMLUtils.searchNode(isCountDataNode)(book)
      .filter(n => getAttribute((n, "cd:type"))
        .filterOrElse(_ == "account", Left()).isRight))
  }

  def extractNumTransactions(book: Node): Either[ValidationError, Int] = {
    case class ExtractNumTransactionsError(msg: String) extends ValidationError
    implicit def errorType: String => ValidationError = ExtractNumTransactionsError.apply


    onlyOne(XMLUtils.searchNode(isCountDataNode)(book)
      .filter(n => getAttribute((n, "cd:type"))
        .filterOrElse(_ == "transaction", Left()).isRight))
  }

  def parseAccountNode(n: Node): Either[ValidationError, Account] = {
    case class ParseAccountNodeError(msg: String) extends ValidationError
    implicit def errorType: String => ValidationError = ParseAccountNodeError.apply

    for {
      _ <- hasNamespace((n, "gnc"))
      v <- getAttribute((n, "version"))
      _ <-
    } yield {

    }
  }

}
