package com.jakway.gnucash.rules

import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.parser.rules.Transaction
import com.jakway.gnucash.parser.xml.ElemReplace
import com.jakway.gnucash.parser.{LinkedAccount, Parser}
import com.jakway.util.Util

import scala.util.matching.Regex
import scala.xml.{Elem, Node, Text}


trait SourceAccountMatcher {
  def sourceAccountMatches(account: LinkedAccount): Boolean
}

abstract class SourceAccountRegex(val sourceAccountPattern: Regex)
  extends SourceAccountMatcher {

  override def sourceAccountMatches(account: LinkedAccount): Boolean =
    //see https://stackoverflow.com/questions/3021813/how-to-check-whether-a-string-fully-matches-a-regex-in-scala
    sourceAccountPattern.pattern.matcher(account.fullName).matches()
}


/**
  *
  * @param pattern
  * @param priority
  * @param sourceAccountPattern a regex object that determines whether a source account matches
  * @param destAccount the account we're transferring to if the rule matches
  */
case class LinkedTransactionRule(ruleName: String,
                                 pattern: Regex,
                                 priority: Double,
                                 override val sourceAccountPattern: Regex,
                                 destAccount: LinkedAccount)
  extends SourceAccountRegex(sourceAccountPattern) {
  import com.jakway.gnucash.parser.xml.NodeTests._

  case class LinkedTransactionRuleError(override val msg: String)
    extends ValidationError(msg)
  implicit def errorType: String => ValidationError = LinkedTransactionRuleError.apply

  /**
    * TODO: handle debits and credits differently
    *
    * re replacing XML, see: https://stackoverflow.com/questions/17705693/scala-replace-xml-element-with-specific-text
    * @param accounts map of account ID -> account
    *                 needed for Parser.isTransaction.
    *                 TODO: refactor so as to make it unnecessary
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
          n.asInstanceOf[Elem].copy(child = Seq(replacementTextNode))
        }
        case _ => n
      }

    } else {
      //leave everything else as-is
      n
    }
  }
}





