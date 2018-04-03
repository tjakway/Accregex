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

/**
  * contains transaction matching and transformation logic
  * @param allAccounts a map of account ID -> LinkedAccount
  * @param targetAccount the account to move transactions from (probably "Unspecified")
  */
class RuleApplicator(val allAccounts: Map[String, LinkedAccount],
                     val targetAccount: LinkedAccount,
                     val rules: Set[LinkedTransactionRule])
  extends ElemReplace[RuleApplicator.RuleApplicatorLogEvent] {
  import RuleApplicator._

  class RuleApplicatorError(override val msg: String)
    extends ValidationError(msg)

  //sanity check
  if(allAccounts.get(targetAccount.id) != targetAccount) {
    throw new RuleApplicatorError("Basic check `allAccounts.get(targetAccount.id) " +
      "!= targetAccount` in RuleApplicator failed!")
  }

  /**
    *
    * @param transactionInput
    * @return the rule to apply or None if no rule matches
    */
  private def whichRule(transactionInput: Transaction): Either[ValidationError, Option[LinkedTransactionRule]] = {
    val matchingRules = rules.filter(ruleMatches(transactionInput))

    //no matches
    if(matchingRules.isEmpty) {
      Right(None)

    //matches found--need to check that there's exactly 1
    //rule to apply
    } else {
      new RuleOrdering(matchingRules.toSeq)
        .getHighestPriority()
        .map(Some(_))
    }
  }

  private def ruleMatches(i: Transaction)(rule: LinkedTransactionRule): Boolean = {
    i.isSourceAccount(rule.sourceAccount) &&
    i.isDestAccount(rule.destAccount) &&
    rule.pattern.findFirstMatchIn(i.description).isDefined
  }

  /**
    * ignore non-transaction nodes and transactions that none of
    * @param e
    * @return
    */
  override def predicateElem(e: Elem): Boolean = {
    Parser.parseTransaction(allAccounts)(e) match {
      case Right(t) => Util.anyOf(rules)(ruleMatches(t))
      case Left(_) => false
    }
  }

  override def replaceElem(e: Elem): (RuleApplicatorLogEvent, Node) = {
    val res = for {
      t <- Parser.parseTransaction(allAccounts)(e)
      optR <- whichRule(t)
      r <- optR match {
        case None => Left(new RuleApplicatorError(s"replaceElem called for " +
          s"$e but no rule matches"))
        case Some(x) => Right(x)
      }
    } yield {
      (r, r.replace(targetAccount.id)(e))
    }

    res match {
      case Left(err) => (RuleApplicatorLogEvent.Error(err), e)
      case Right((rule, newNode)) => {
        val oldNode = e
        (RuleApplicatorLogEvent.Success(rule, oldNode), newNode)
      }
    }
  }
}

object RuleApplicator {

  class RuleOrdering(override val toOrder: Seq[LinkedTransactionRule])
    extends ZeroHighPriority[LinkedTransactionRule](toOrder) {

    override def getPriority(obj: LinkedTransactionRule): Double = obj.priority
  }

  sealed trait RuleApplicatorLogEvent

  object RuleApplicatorLogEvent {
    case class Error(validationError: ValidationError)
      extends RuntimeException(s"$validationError")
        with RuleApplicatorLogEvent

    case class Success(ruleApplied: LinkedTransactionRule, node: Node)
      extends RuleApplicatorLogEvent
  }

}

