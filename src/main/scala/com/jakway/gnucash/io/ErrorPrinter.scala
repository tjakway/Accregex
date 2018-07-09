package com.jakway.gnucash.io

import autogen.buildinfo.BuildInfo
import com.jakway.gnucash.error.ValidationError
import com.jakway.util.StackTraceString

object ErrorPrinter {
  def format(validationError: ValidationError, includeIssueTrackerMessage: Boolean = true): String = {
    val suppressedExceptions = {
      val suppressed = validationError.getSuppressed()
      if(suppressed.isEmpty) {
        "\t\tNone"
      } else {
        suppressed
          .map(formatThrowable)
          .foldLeft("\t\t")(_ + _)
      }
    }

    val issueTrackerMessage =
      if(includeIssueTrackerMessage) {
        s"Please report the following information to ${BuildInfo.issueTracker}:\n"
      } else {
        ""
      }

    s"An error of type ${validationError.getClass.getCanonicalName} occurred with message " +
      s"${validationError.msg}\n\n" +
      issueTrackerMessage +
      "\tStack Trace: \n" +
      "\t\t" + validationError.stackTrace +
      "\tSuppressed Exceptions: \n" + suppressedExceptions
  }

  private def formatThrowable(t: Throwable): String =
    s"Throwable of type ${t.getClass().getCanonicalName}" +
      " with message " + t.getMessage + " with stack trace " +
      StackTraceString.stackTraceToString(t)

  def formatMultipleErrorMessages(msgs: Seq[String]) = {
    msgs.zipWithIndex.map {
      case (msg, index) => s"$index: $msg"
    }.reduce(_ + System.lineSeparator() + _)
  }
}
