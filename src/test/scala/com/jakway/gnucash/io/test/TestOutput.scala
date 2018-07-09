package com.jakway.gnucash.io.test

import java.io.File

import com.jakway.gnucash.error.{MultiValidationError, ValidationError}
import com.jakway.gnucash.test.IntegrationTests.HasFoodTestConf
import com.jakway.gnucash.test.ResourceFiles
import com.jakway.util.Util
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

object TestOutput {
  case class TestOutputException(override val msg: String)
    extends ValidationError(msg)

  def checkAndDeleteFile(checks: Seq[File => Either[String, Unit]])(f: => File): Unit = {

    val readableFileChecks: Seq[File => Either[String, Unit]] = {
      def fExists(f: File) =
        if(!f.exists) {
          Left(s"$f does not exist")
        } else Right(())

      def canRead(f: File) =
        if(!f.canRead) {
          Left(s"Cannot read file $f")
        } else {
          Right(())
        }

      def isNotDirectory(f: File) =
        if(f.isDirectory) {
          Left(s"$f is a directory")
        } else {
          Right(())
        }

      Seq(fExists, canRead, isNotDirectory)
    }

    val allChecks = checks ++ readableFileChecks

    val checkResults = Util.checkAll(TestOutputException.apply)(allChecks)(f)

    //delete the file
    //if we can't delete it combine that error with any previous errors
    if(!f.delete()) {
      val couldNotDeleteMsg = s"Could not delete $f"

      checkResults match {
        case Left(errs) => TestOutputException(s"At least 2 errors occurred: `${errs.msg}` then " +
          s"`$couldNotDeleteMsg`")
        case Right(()) => TestOutputException(couldNotDeleteMsg)
      }
    } else {
      checkResults
    }
  }
}
import TestOutput._

class TestOutput
  extends FlatSpec
    with Matchers
    with ResourceFiles
    with HasFoodTestConf {

  "Output" should "write the food test output to file" in {
    assertAndDeleteFile()
  }
}
