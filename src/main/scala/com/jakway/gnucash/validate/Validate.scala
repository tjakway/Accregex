package com.jakway.gnucash.validate

import com.jakway.util.XMLUtils

import scala.util.Try
import scala.xml.Node

/**
  * TODO:
  * -check count-data attribute cd:type=="book" && text=="1"
  *
  * -
  */
object Validate {
  import NodeTests._

  def findBookNode(root: Node): ValidateF[Node, Node] =
    (root, errorType: String => ValidationError) => {
      def isBookNode(n: Node): Boolean = {
        n.label == "book" &&
         getAttribute((n, "version")).isRight &&
         hasNamespace((n, "gnc")).isRight
      }

      findOnlyOne((isBookNode _, root))
    }

  def isCountDataNode(n: Node): Boolean = {
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

  def apply(root: Node) = {

  }
}
