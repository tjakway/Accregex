package com.jakway.gnucash.validate

import scala.xml.Node

/**
  * TODO:
  * -check count-data attribute cd:type=="book" && text=="1"
  *
  * -
  */
object Validate {

  private def isBook(node: Node): Boolean = {
    node.namespace == "gnc" && node.label == "book"
  }



  def extractNumAccounts(book: Node): Either[ValidationError, Int] = {
    case class ExtractNumAccountsError(msg: String) extends ValidationError

    ???
  }

  def apply(root: Node) = {

  }
}
