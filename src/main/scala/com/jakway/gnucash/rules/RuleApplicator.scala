package com.jakway.gnucash.rules

import java.util.Formatter

import com.jakway.gnucash.Config
import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.parser.rules.Transaction
import com.jakway.gnucash.parser.xml.ElemReplace
import com.jakway.gnucash.parser.{LinkedAccount, Parser}
import com.jakway.util.Util

import scala.xml.{Elem, Node}

/**
  * contains transaction matching and transformation logic
 *
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
  if(allAccounts.get(targetAccount.id) == targetAccount) {
    throw new RuleApplicatorError("Basic check `allAccounts.get(targetAccount.id) " +
      "== targetAccount` in RuleApplicator failed!")
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
    //check if this transaction credits funds to the TARGET
    //account (i.e. the one we want to replace)
    i.isDestAccount(targetAccount) &&
    rule.pattern.findFirstMatchIn(i.description).isDefined
  }

  /**
    * ignore non-transaction nodes and transactions that none of
    * @param e
    * @return
    */
  protected override def predicateElem(e: Elem): Boolean = {
    Parser.parseTransaction(allAccounts)(e) match {
      case Right(t) => Util.anyOf(rules)(ruleMatches(t))
      case Left(_) => false
    }
  }

  protected override def replaceElem(e: Elem): (RuleApplicatorLogEvent, Node) = {
    val res = for {
      t <- Parser.parseTransaction(allAccounts)(e)
      optR <- whichRule(t)
      r <- optR match {
        case None => Left(new RuleApplicatorError(s"replaceElem called for " +
          s"$e but no rule matches"))
        case Some(x) => Right(x)
      }
    } yield {
      (r, t, r.replace(allAccounts, targetAccount.id)(e))
    }

    res match {
      case Left(err) => (RuleApplicatorLogEvent.Error(err), e)
      case Right((rule, transaction, newNode)) => {
        val oldNode = e
        (RuleApplicatorLogEvent.Success(rule, transaction, targetAccount, oldNode, newNode), newNode)
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

    case class Success(ruleApplied: LinkedTransactionRule,
                       transaction: Transaction,
                      /* the transaction's original account (most likely Unspecified/Imbalance or similar) */
                       targetAccount: LinkedAccount,
                       oldNode: Node,
                       newNode: Node)
      extends RuleApplicatorLogEvent

    class Counts(events: Set[RuleApplicatorLogEvent]) {
      val hasErrorEvents: Boolean = Util.anyOf(events) {
        //check if there are any errors events
        case RuleApplicatorLogEvent.Error(_) => true
        case _ => false
      }

      //group log events by type
      val eventsGrouped = {
        val empty: (Set[RuleApplicatorLogEvent], Set[RuleApplicatorLogEvent]) = (Set(), Set())
        events.foldLeft(empty) {
          case ((successes, errors), e: RuleApplicatorLogEvent.Error) =>
            (successes, errors + e)

          case ((successes, errors), e: RuleApplicatorLogEvent.Success) =>
            (successes + e, errors)
        }
      }

      val eventCounts: (Int, Int) =
        (eventsGrouped._1.size, eventsGrouped._2.size)

      val successEvents = eventsGrouped._1
      val errorEvents = eventsGrouped._2

      val numSuccessEvents = eventCounts._1
      val numErrorEvents = eventCounts._2
      val percentErrors: Double = (numErrorEvents.toDouble) / (events.size.toDouble)
    }
  }
}