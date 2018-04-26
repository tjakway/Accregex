package com.jakway.gnucash.io

import java.util.Formatter

import com.jakway.gnucash.rules.RuleApplicator
import com.jakway.gnucash.{Config, ValidatedConfig}
import com.jakway.gnucash.rules.RuleApplicator.RuleApplicatorLogEvent
import com.jakway.util.Util

class RuleApplicatorEventPrinter(val verbosity: Config.Verbosity,
                                 val events: Set[RuleApplicatorLogEvent]) {


  val counts = new RuleApplicator.RuleApplicatorLogEvent.Counts(events)

  /**
    * truncate strings that are too long and append "..." to the end of them
    * @param s
    * @return
    */
  private def truncateString(fmt: Formatter)(s: String): String = {
    if(s.length <= verbosity.transactionMemoMaxLength) {
      s
    } else {
      s.substring(0, verbosity.transactionMemoMaxLength - verbosity.ellipses.length) +
        verbosity.ellipses
    }
  }

  private def formatLogEvents(fmt: Formatter)(events: Set[RuleApplicatorLogEvent]): String = {
    if(events.isEmpty) {
      "No changes were applied."
    } else {
      //TODO
    }
  }


  private def drawHorizontalLine(fmt: Formatter, unit: String = "*"): Unit = {

    fmt.format("%-s%n", (1 to verbosity.lineWidth).map(_ => unit))
  }

  private def formatCounts(fmt: Formatter): Unit = {
    fmt.format("%-s: %d", "Transactions successfully changed: ", counts.numSuccessEvents)
    fmt.format("%-s: %d", "Transaction change errors: ", counts.numErrorEvents)
    fmt.format("%d errors occurred out of %d events total (%%%,05.2f)%n",
      counts.numSuccessEvents, events.size, counts.percentErrors)

    if(counts.numErrorEvents == 0) {
      fmt.format("%-s%n", "No errors encountered!")
    }
  }

  private def formatSummary(fmt: Formatter): Unit = {
    fmt.format("Below is a summary of changes made to the output file:%n")

    drawHorizontalLine(fmt)
    formatCounts(fmt)
  }

  private def formatErrorMessage(fmt: Formatter): Unit = {
    counts.errorEvents.zipWithIndex.foreach {
      case (RuleApplicatorLogEvent.Error(e), index) => {
        drawHorizontalLine(fmt)

        //left-justify the header to be consistent with the horizontal lines
        val header = String.format("***** Error number %d *****", index)
        fmt.format("%-s%n", header)
        drawHorizontalLine(fmt, "-")

        fmt.format("%s", ErrorPrinter.format(e))

        drawHorizontalLine(fmt)
      }
    }

    drawHorizontalLine(fmt)
    formatCounts(fmt)
  }

  def format(): String = {
    //for reasons unknown StringBuilder generated a compile
    //error when used as an Appendable
    //see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4983949 for the bug report on the issue
    //the error seems to persist despite being listed as fixed
    val buf: Appendable = new StringBuffer()
    val fmt = new Formatter(buf)

    if(counts.hasErrorEvents) {
      formatErrorMessage(fmt)
    } else {
      formatSummary(fmt)
    }

    fmt.toString()
  }


  def print(): Unit = {
    val str = format()

    //print to stderr or stdout as appropriate
    if(!str.isEmpty) {
      if(counts.hasErrorEvents) {
        System.err.print(str)
      } else {
        System.out.print(str)
      }
    }
  }
}
