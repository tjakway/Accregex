package com.jakway.gnucash.io

import java.util.Formatter

import com.jakway.gnucash.{Config, ValidatedConfig}
import com.jakway.gnucash.rules.RuleApplicator.RuleApplicatorLogEvent
import com.jakway.util.Util

class RuleApplicatorEventPrinter(val verbosity: Config.Verbosity,
                                 val events: Set[RuleApplicatorLogEvent]) {

  lazy val hasErrorEvents: Boolean = Util.anyOf(events) {
    //check if there are any errors events
    case RuleApplicatorLogEvent.Error(_) => true
    case _ => false
  }

  lazy val eventCounts: (Int, Int) =
    (eventsGrouped._1.size, eventsGrouped._2.size)

  //group log events by type
  lazy val eventsGrouped = {
    val empty: (Set[RuleApplicatorLogEvent], Set[RuleApplicatorLogEvent]) = (Set(), Set())
    events.foldLeft(empty) {
      case ((successes, errors), e: RuleApplicatorLogEvent.Error) =>
        (successes, errors + e)

      case ((successes, errors), e: RuleApplicatorLogEvent.Success) =>
        (successes + e, errors)
    }
  }

  lazy val successEvents = eventsGrouped._1
  lazy val errorEvents = eventsGrouped._2

  lazy val numSuccessEvents = eventCounts._1
  lazy val numErrorEvents = eventCounts._2
  lazy val percentErrors: Double = (numErrorEvents.toDouble) / (events.size.toDouble)

  /*********************************************/


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

    //left-justify the line
    fmt.format("%-s%n", (1 to verbosity.lineWidth).map(_ => unit))
  }

  private def formatCounts(fmt: Formatter): Unit = {
    fmt.format("%-s: %d", "Transactions successfully changed: ", numSuccessEvents)
    fmt.format("%-s: %d", "Transaction change errors: ", numErrorEvents)
    fmt.format("%d errors occurred out of %d events total (%%%,05.2f)%n",
      numSuccessEvents, events.size, percentErrors)

    if(numErrorEvents == 0) {
      fmt.format("%-s%n", "No errors encountered!")
    }
  }

  private def formatSummary(fmt: Formatter): Unit = {
    fmt.format("Below is a summary of changes made to the output file:%n")

    drawHorizontalLine(fmt)
    formatCounts(fmt)
  }

  private def formatErrorMessage(fmt: Formatter): Unit = {
    errorEvents.zipWithIndex.foreach {
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

    if(hasErrorEvents) {
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
      if(hasErrorEvents) {
        System.err.print(str)
      } else {
        System.out.print(str)
      }
    }
  }
}
