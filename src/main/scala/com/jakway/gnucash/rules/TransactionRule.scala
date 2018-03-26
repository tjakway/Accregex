package com.jakway.gnucash.rules

import java.util.regex.PatternSyntaxException

import com.jakway.gnucash.parser.{LinkedAccount, ValidationError}

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

case class UnlinkedTransactionRule(pattern: String,
                                   priority: String,
                                   sourceAccount: String,
                                   destAccount: String)

case class LinkedTransactionRule(pattern: Regex,
                                 priority: Double,
                                 sourceAccount: LinkedAccount,
                                 destAccount: LinkedAccount)



object UnlinkedTransactionRule {
  def link(accounts: Seq[LinkedAccount])
          (rule: UnlinkedTransactionRule):
          Either[ValidationError, LinkedTransactionRule] = {
    case class LinkTransactionRuleError(override val msg: String) extends ValidationError(msg)
    implicit def errorType: String => ValidationError = LinkTransactionRuleError.apply

    import com.jakway.gnucash.parser.ValidateF.MiscTests._

    def compileRegex(r: String): Either[ValidationError, Regex] = {
      val rgxMsg = s"$r is an invalid regular expression.  This program follows java regular" +
        s" expression conventions; see " +
        "https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html " +
        "for more information."

      val unknownErrorMsg = s"Unknown error while compiling regular expression $r, see" +
        s" included exception for the cause"

      Try(r.r) match {
        case Success(rgx) => Right(rgx)
        case Failure(PatternSyntaxException) => Left(errorType(rgxMsg))
        case Failure(t) => Left(
          ValidationError.fromCause[LinkTransactionRuleError](unknownErrorMsg, t))
      }
    }

    rule match {
      case UnlinkedTransactionRule(pattern, priority, sourceAccount, destAccount) => {
        for {
          priorityNum <- isNumeric(priority)
          rgx <- compileRegex(pattern)

        } yield {

        }
      }
    }
  }
}