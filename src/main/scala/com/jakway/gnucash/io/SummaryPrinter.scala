package com.jakway.gnucash.io

import java.util.Formatter

import com.jakway.gnucash.{Config, ValidatedConfig}
import com.jakway.gnucash.rules.RuleApplicator.RuleApplicatorLogEvent

class SummaryPrinter(val verbosity: Config.Verbosity) {
  /**
    * truncate strings that are too long and append "..." to the end of them
    * @param s
    * @return
    */
  private def truncateString(s: String): String = {
    if(s.length <= verbosity.transactionMemoMaxLength) {
      s
    } else {
      s.substring(0, verbosity.transactionMemoMaxLength - verbosity.ellipses.length) +
        verbosity.ellipses
    }
  }

  private def formatLogEvents(events: Seq[RuleApplicatorLogEvent]): String = {
    if(events.isEmpty) {
      "No changes were applied."
    } else {
      //TODO

    }
  }

  def formatSummary(events: Seq[RuleApplicatorLogEvent]): String = {
    //for reasons unknown StringBuilder generated a compile
    //error when used as an Appendable
    //see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4983949 for the bug report on the issue
    //the error seems to persist despite being listed as fixed
    val buf: Appendable = new StringBuffer()
    val fmt = new Formatter(buf)

    if(verbosity.printSummary) {
      fmt.format("Below is a summary of changes made to the output file:%n")
    }

    fmt.toString()
  }
}
