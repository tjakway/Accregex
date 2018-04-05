package com.jakway.gnucash.rules

import com.jakway.gnucash.parser.rules.Transaction
import com.jakway.gnucash.parser.xml.ElemReplace
import com.jakway.gnucash.parser.{LinkedAccount, Parser, ValidationError}
import com.jakway.util.Util

import scala.util.matching.Regex
import scala.xml.{Elem, Node, Text}


/**
  *
  * @param pattern
  * @param priority
  * @param sourceAccount
  * @param destAccount the account we're transferring to if the rule matches
  */
case class LinkedTransactionRule(pattern: Regex,
                                 priority: Double,
                                 sourceAccount: LinkedAccount,
                                 destAccount: LinkedAccount) {
  import com.jakway.gnucash.parser.xml.NodeTests._

  case class LinkedTransactionRuleError(override val msg: String)
    extends ValidationError(msg)
  implicit def errorType: String => ValidationError = LinkedTransactionRuleError.apply


  /**
    * TODO: handle debits and credits differently
    *
    * re replacing XML, see: https://stackoverflow.com/questions/17705693/scala-replace-xml-element-with-specific-text
    * @param accounts needed for Parser.isTransaction.  TODO: refactor so as to make it unnecessary
    * @param targetAccountID most likely "unspecified"
    * @param n
    * @return
    */
  def replace(accounts: Map[String, LinkedAccount], targetAccountID: String)
             (n: Node): Node = {

    def recurse = n
      .asInstanceOf[Elem].copy(child = n.child.map(replace(accounts, targetAccountID)))

    lazy val splitAccountNode = Parser.getSplitAccount(n)

    if (Parser.isTransaction(accounts)(n)) {
      recurse
    } else if (Parser.isSplits(n)) {
      recurse
    } else if (Parser.isSplit(n)) {
      recurse
    } else if (splitAccountNode.isRight) {
      splitAccountNode match {
        case Right(accountId)
          if accountId == targetAccountID => {

          //the new account text node
          val replacementTextNode = Text(destAccount.id)

          //return a copy with the new child node
          splitAccountNode
            //already checked it's a Right
            .right.get
            .asInstanceOf[Elem]
            .copy(child = Seq(replacementTextNode))
        }
        case _ => n
      }

    } else {
      //leave everything else as-is
      n
    }
  }
}





