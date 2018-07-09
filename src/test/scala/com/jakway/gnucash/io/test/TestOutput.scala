package com.jakway.gnucash.io.test

import java.io.File

import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.test.IntegrationTests.HasFoodTestConf
import com.jakway.gnucash.test.ResourceFiles
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

object TestOutput {
  case class TestOutputException(override val msg: String)
    extends ValidationError(msg)

  def checkAndDeleteFile(checks: Seq[File => Option[String]])(f: => File): Unit = {
    val ex: Option[ValidationError] = {
      //should be a regular file
      if(!f.exists()) {
        Some(s"$f does not exist")
      } else if(!f.canRead) {
        Some(s"Cannot read file $f")
      } else if(f.isDirectory) {
        Some(s"$f is a directory")
      } else {
        None
      }
    }.map(TestOutputException.apply)

    //delete the file
    //if we can't delete it combine that error with any previous errors
    val deleteRes =
      if(!f.delete()) {
        val newEx = TestOutputException(s"Could not delete $f")

        Some(ex.map(oldEx => TestOutputException(s"2 errors occurred: `${oldEx.msg}` then " +
          s"`${newEx.msg}`")).getOrElse(newEx))
      } else {
        None
      }

    //throw any exceptions we found
    deleteRes match {
      case Some(ex) => throw ex
      case None => Unit
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
