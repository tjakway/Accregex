package com.jakway.gnucash.rules

import com.jakway.gnucash.parser.rules.{Transaction, Split}
import com.jakway.gnucash.parser.{LinkedAccount, ValidationError}

import scala.util.matching.Regex


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
                                 destAccount: LinkedAccount)


/**
  * contains transaction matching and transformation logic
  * @param destAccount the account to move transactions from (probably "Unspecified")
  */
class RuleApplicator(val destAccount: LinkedAccount, val rules: Set[LinkedTransactionRule]) {
  import RuleApplicator._

  class RuleApplicatorError(override val msg: String)
    extends ValidationError(msg)

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

}

object RuleApplicator {

  class RuleOrdering(override val toOrder: Seq[LinkedTransactionRule])
    extends ZeroHighPriority[LinkedTransactionRule](toOrder) {

    override def getPriority(obj: LinkedTransactionRule): Double = obj.priority
  }
}

