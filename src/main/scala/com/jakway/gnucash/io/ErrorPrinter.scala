package com.jakway.gnucash.io

import autogen.buildinfo.BuildInfo
import com.jakway.gnucash.parser.ValidationError
import com.jakway.util.StackTraceString

object ErrorPrinter {
  def format(validationError: ValidationError): String = {
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

    s"An error of type ${validationError.getClass.getCanonicalName} occurred with message " +
      s"${validationError.msg}\n\n" +
      s"Please report the following information to ${BuildInfo.issueTracker}:" +
      "\n" +
      "\n" +
      "\tStack Trace: \n" +
      "\t\t" + validationError.stackTrace +
      "\tSuppressed Exceptions: \n" + suppressedExceptions
  }

  private def formatThrowable(t: Throwable): String =
    s"Throwable of type ${t.getClass().getCanonicalName}" +
      " with message " + t.getMessage + " with stack trace " +
      StackTraceString.stackTraceToString(t)
}
