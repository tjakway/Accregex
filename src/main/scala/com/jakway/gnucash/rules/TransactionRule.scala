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
    * @param targetAccountID most likely "unspecified"
    * @param n
    * @return
    */
  def replace(targetAccountID: String)(n: Node): Node = n match {
      //recurse downward on all the relevant split nodes until
      //we get to split:account
    case <transaction>{ ch @ _*}</transaction> =>
      <transaction>{ch.map(replace(targetAccountID))}</transaction>

    case <splits>{ ch @ _*}</splits> => <splits>{ch.map(replace(targetAccountID))}</splits>
    case <split>{ch @ _*}</split> => <split>{ch.map(replace(targetAccountID))}</split>

      //do the actual replacement
    case accountNode @ <account>{contents}</account>
      if hasNamespace((accountNode, "split")).isRight &&
          expectAttribute((accountNode, "type", "guid")).isRight &&
          contents == targetAccountID => {

      //make sure there isn't anything there we don't expect to find
      assert(contents == Seq(Text(targetAccountID)))
      assert(accountNode.child == contents)

      val replacement = Seq(Text(destAccount.id))
      accountNode.asInstanceOf[Elem].copy(child = replacement)
    }

      //leave everything else as-is
    case o @ _ => o
  }
}





