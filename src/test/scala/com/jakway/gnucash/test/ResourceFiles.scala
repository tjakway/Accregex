package com.jakway.gnucash.test

import java.io.File

import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.util.StreamReader
import com.jakway.util.error.{UsesTempDir, WithCause}

object ResourceFiles {
  case class ResourceFilesError(override val msg: String)
    extends AccregexTestSetupException(msg)
}

trait ResourceFiles extends UsesTempDir[ResourceFiles.ResourceFilesError] {
  override val tempDirParam: Option[File] = None
  override val defaultTempDirPrefix: String = "accregexresourcefiles"
  override def usesTempDirErrorTypeCTOR: String => ResourceFiles.ResourceFilesError =
    ResourceFiles.ResourceFilesError.apply

  lazy val tempDir: File = getTempDir().right.get

  private def getResourceStream(name: String) = getClass().getResourceAsStream(name)
  private def readResource(name: String): String = {
    implicit def errorType: String => ValidationError = ((msg: String) => new ValidationError(
                                      s"Could not read resource $name, error message: " + msg))
    StreamReader.readStream(getResourceStream(name))
      .right.get
  }

  def copyResourceToFile(name: String): File =
    stringToTempFile(tempDir)(readResource(name)).right.get
}
