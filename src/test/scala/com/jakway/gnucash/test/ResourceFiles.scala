package com.jakway.gnucash.test

import java.io.File
import java.nio.file.Files

import com.jakway.gnucash.error.{ValidateUsesTempDir, ValidationError}
import com.jakway.util.StreamReader

import scala.collection.JavaConverters


trait ResourceNames {
  val regDocXML = "/reg_doc_example.gnucash"
  val currencyTreeXML = "/currency_tree_xml.gnucash"

  val foodTest = "/food_test.gnucash"
  val foodTestExpected = "/food_test_expected.gnucash"
  val foodTestRules = "/food_test_rules.json"
}

object ResourceFiles {
  case class ResourceFilesError(override val msg: String)
    extends AccregexTestSetupException(msg)


}


trait ResourceFiles extends ValidateUsesTempDir with ResourceNames {
  override val tempDirParam: Option[File] = None
  override val defaultTempDirPrefix: String = "accregexresourcefiles"
  override def usesTempDirErrorTypeCTOR: String => ResourceFiles.ResourceFilesError =
    ResourceFiles.ResourceFilesError.apply

  import ResourceFiles._

  protected val checkCopies: Boolean = true

  lazy val tempDir: File = getTempDir().right.get

  private def getResourceStream(name: String) = getClass().getResourceAsStream(name)

  private def readResource(name: String): String = {
    implicit def errorType: String => ValidationError = (msg: String) => new ValidationError(
                                      s"Could not read resource $name, error message: " + msg)
    StreamReader.readStream(getResourceStream(name))
      .right.get
  }

  private def readResourceAsSource(name: String): String = {
    scala.io.Source.fromInputStream(getResourceStream(name), "UTF-8").mkString
  }

  private def readFile(file: File): String = {
    JavaConverters.asScalaBuffer(Files.readAllLines(file.toPath))
      .reduce(_ + System.lineSeparator() + _)
  }

  private def checkReadResource(name: String): String = {
    val res = readResource(name)
    val resSource = readResourceAsSource(name)

    if(res != resSource) {
      throw new ResourceFilesError("readResource returned different" +
        s" value for resource named $name: $res vs $resSource")
    } else {
      res
    }
  }

  def copyResourceToFile(name: String): File = {
    val res = if(checkCopies) {
      checkReadResource(name)
    } else {
      readResource(name)
    }

    val outFile = stringToTempFile(tempDir)(res).right.get

    if(checkCopies) {
      val read = readFile(outFile)
      if(read != res) {
        throw new ResourceFilesError("Error in stringToTempFile," +
          s" expected to write $res but read back $read")
      } else {
        outFile
      }
    } else {
      outFile
    }
  }
}
