package com.jakway.gnucash.io

import java.util.{Formatter, Locale}

import com.jakway.gnucash.parser.{LinkedAccount, ValidationError}
import com.jakway.gnucash.parser.rules.Transaction
import com.jakway.gnucash.rules.{LinkedTransactionRule, RuleApplicator}
import com.jakway.gnucash.{Config, ValidatedConfig}
import com.jakway.gnucash.rules.RuleApplicator.RuleApplicatorLogEvent
import com.jakway.util.{Util, XMLUtils}

object RuleApplicatorEventPrinter {
  case class FormatLogEventsError(override val msg: String)
    extends ValidationError(msg)

}

class RuleApplicatorEventPrinter(val verbosity: Config.Verbosity,
                                 val events: Set[RuleApplicatorLogEvent]) {

  import RuleApplicatorEventPrinter._

  val counts = new RuleApplicator.RuleApplicatorLogEvent.Counts(events)

  //for reasons unknown StringBuilder generated a compile
  //error when used as an Appendable
  //see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4983949 for the bug report on the issue
  //the error seems to persist despite being listed as fixed
  val _fmt = {
    val buf: Appendable = new StringBuffer()
    new Formatter(buf)
  }
  val locale = Locale.getDefault()

  private def fmt(fmtStr: String, args: Any*) =
    _fmt.format(locale, fmtStr, args)



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

  private def rightArrow(verbosity: Config.Verbosity): String =
    if(verbosity.useUnicodeArrow) {
      //see https://stackoverflow.com/questions/5585919/creating-unicode-character-from-its-number
      //for getting a unicode character from its integer representation
      new String(Character.toChars(0x2192))
    } else {
      "->"
    }

  private def accountName(l: LinkedAccount): String =
    if(verbosity.useAccountFullName) {
      l.fullName
    } else {
      l.name
    }

  private def formatSuccessfulLogEvents(): Unit = {
    if(counts.successEvents.isEmpty) {
      fmt("No changes were applied.")
    } else {
      counts.successEvents.foreach {
        case RuleApplicatorLogEvent.Success(ruleApplied: LinkedTransactionRule,
                                            transaction: Transaction,
                                            targetAccount: LinkedAccount,
                                            oldNode: scala.xml.Node,
                                            newNode: scala.xml.Node) => {


          fmt("Rule '%-s' changed transaction '%-s':\t%s %s %s%n",
            ruleApplied.ruleName,
            truncateString(transaction.description),

            accountName(targetAccount),
            rightArrow(verbosity),
            accountName(ruleApplied.destAccount))

          //print a lot more stuff if this debugging flag is on
          if(verbosity.printModifiedTransactionNodes) {
            fmt("\t%s, %s, %s%n",
              ruleApplied,
              XMLUtils.nodeToString(oldNode).right.getOrElse(s"ERROR FORMATTING NODE <$oldNode>"),
              XMLUtils.nodeToString(newNode).right.getOrElse(s"ERROR FORMATTING NODE <$newNode>"))
          }
        }

        case other => throw FormatLogEventsError(s"formatLogEvents expected only successful log events but got " +
          s"$other")
      }
    }
  }


  private def drawHorizontalLine(unit: String = "*"): Unit = {

    fmt("%-s%n", (1 to verbosity.lineWidth).map(_ => unit))
  }

  private def formatCounts(): Unit = {
    fmt(s"%-s: %d", "Transactions successfully changed: ", counts.numSuccessEvents)
    fmt("%-s: %d", "Transaction change errors: ", counts.numErrorEvents)
    fmt("%d errors occurred out of %d events total (%%%,05.2f)%n",
      counts.numSuccessEvents, events.size, counts.percentErrors)

    if(counts.numErrorEvents == 0) {
      fmt("%-s%n", "No errors encountered!")
    }
  }

  private def formatSummary(): Unit = {
    fmt("Below is a summary of changes made to the output file:%n")

    drawHorizontalLine()
    formatCounts()
  }

  private def formatErrorMessage(): Unit = {
    counts.errorEvents.zipWithIndex.foreach {
      case (RuleApplicatorLogEvent.Error(e), index) => {
        drawHorizontalLine()

        //left-justify the header to be consistent with the horizontal lines
        val header = String.format(locale, "***** Error number %d *****", index.asInstanceOf[Object])
        fmt("%-s%n", header)
        drawHorizontalLine("-")

        fmt("%s", ErrorPrinter.format(e))

        drawHorizontalLine()
      }

      case r => throw new ValidationError(s"Expected error event but got $r")
    }

    drawHorizontalLine()
    formatCounts()
  }

  def format(): String = {

    if(counts.hasErrorEvents) {
      formatErrorMessage()
    } else {
      formatSummary()
    }

    _fmt.toString()
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
