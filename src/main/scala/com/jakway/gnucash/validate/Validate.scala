package com.jakway.gnucash.validate

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
        for {
          _ <- hasAttribute((n, "version"))

        }
      }
    }


  def extractNumAccounts(book: Node): Either[ValidationError, Int] = {
    case class ExtractNumAccountsError(msg: String) extends ValidationError

    ???
  }

  def apply(root: Node) = {

  }
}
