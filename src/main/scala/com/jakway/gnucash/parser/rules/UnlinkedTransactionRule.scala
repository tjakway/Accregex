package com.jakway.gnucash.parser.rules

import java.util.regex.PatternSyntaxException

import com.jakway.gnucash.error.{ValidateF, ValidationError}
import com.jakway.gnucash.parser.AccountNameParser
import com.jakway.gnucash.rules.LinkedTransactionRule

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

/**
  * the raw output of parsing a rule from the user's json config
  * @param pattern
  * @param priority
  * @param sourceAccount
  * @param destAccount
  */
case class UnlinkedTransactionRule(ruleName: String,
                                   pattern: String,
                                   priority: String,
                                   sourceAccount: String,
                                   destAccount: String)

object UnlinkedTransactionRule {
  val defaultPriority = 0

  def link(accountNameParser: AccountNameParser)
          (rule: UnlinkedTransactionRule):
          Either[ValidationError, LinkedTransactionRule] = {
    case class LinkTransactionRuleError(override val msg: String) extends ValidationError(msg)
    implicit def errorType: String => ValidationError = LinkTransactionRuleError.apply

    def compileRegex(r: String): Either[ValidationError, Regex] = {
      val rgxMsg = s"$r is an invalid regular expression.  This program follows java regular" +
        s" expression conventions; see " +
        "https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html " +
        "for more information."

      val unknownErrorMsg = s"Unknown error while compiling regular expression $r, see" +
        s" included exception for the cause"

      Try(r.r) match {
        case Success(rgx) => Right(rgx)
        case Failure(_: PatternSyntaxException) => Left(errorType(rgxMsg))
        case Failure(t) => Left(
          ValidationError.fromCause(unknownErrorMsg, t))
      }
    }

    rule match {
      case UnlinkedTransactionRule(ruleName, pattern, priority, sourceAccount, destAccount) => {
        for {
          priorityNum <- ValidateF.MiscTests.isNumeric(priority)
          rgx <- compileRegex(pattern)
          src <- accountNameParser.findReferencedAccount(sourceAccount)
          dst <- accountNameParser.findReferencedAccount(destAccount)
        } yield {
          LinkedTransactionRule(ruleName, rgx, priorityNum, src, dst)
        }
      }
    }
  }
}