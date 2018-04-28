package com.jakway.gnucash.test

import java.io.File

import com.jakway.gnucash.error.{ValidateUsesTempDir, ValidationError}
import com.jakway.gnucash.util.StreamReader

object ResourceFiles {
  case class ResourceFilesError(override val msg: String)
    extends AccregexTestSetupException(msg)
}

trait ResourceNames {
  val regDocXML = "/reg_doc_example.gnucash"
  val currencyTreeXML = "/currency_tree_xml.gnucash"

  val foodTest = "/food_test.gnucash"
  val foodTestExpected = "/food_test_expected.gnucash"
  val foodTestRules = "/food_test_rules.json"
}

trait ResourceFiles extends ValidateUsesTempDir with ResourceNames {
  override val tempDirParam: Option[File] = None
  override val defaultTempDirPrefix: String = "accregexresourcefiles"
  override def usesTempDirErrorTypeCTOR: String => ResourceFiles.ResourceFilesError =
    ResourceFiles.ResourceFilesError.apply

  lazy val tempDir: File = getTempDir().right.get

  private def getResourceStream(name: String) = getClass().getResourceAsStream(name)
  private def readResource(name: String): String = {
    implicit def errorType: String => ValidationError = (msg: String) => new ValidationError(
                                      s"Could not read resource $name, error message: " + msg)
    StreamReader.readStream(getResourceStream(name))
      .right.get
  }

  def copyResourceToFile(name: String): File =
    stringToTempFile(tempDir)(readResource(name)).right.get
}
