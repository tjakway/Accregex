package com.jakway.gnucash.io

import java.io._
import java.nio.file.Files

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.io.XMLValidator.XMLValidationError
import com.jakway.gnucash.parser.{ValidateF, ValidateUsesTempDir, ValidationError}
import com.jakway.util.XMLUtils
import com.jakway.util.error.UsesTempDir
import com.jakway.util.runner._
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{Schema, SchemaFactory}

import scala.util.{Failure, Success, Try}

object XMLValidator {
  class XMLValidationError(override val msg: String)
    extends ValidationError(msg)

  def getValidators(conf: ValidatedConfig): (XMLValidator, XMLValidator) = {
    lazy val tempDir = conf.tempDir.map(new File(_))

    val inputValidator = if(conf.skipInputValidation) {
      new SkipXMLValidator()
    } else {
      new XMLLintValidator(conf.verbosity.debug, tempDir)
    }

    val outputValidator = if(conf.skipOutputValidation) {
      new SkipXMLValidator()
    } else {
      //if we're performing both input and output verification then
      //use the same validator object for both
      if(inputValidator.isInstanceOf[XMLLintValidator]) {
        inputValidator
      } else {
        new XMLLintValidator(conf.verbosity.debug, tempDir)
      }
    }

    (inputValidator, outputValidator)
  }

  val schemaName = "gnucash-v2.rng"
  val schemaResource = "/" + schemaName
}

trait XMLValidator {
  def validateNode(inputName: String, node: scala.xml.Node): Either[ValidationError, Unit] =
    XMLUtils.nodeToString(node).flatMap(validate(inputName, _))

  def validate(inputName: String, xmlInput: String): Either[ValidationError, Unit] = {
    validate(inputName, new ByteArrayInputStream(xmlInput.getBytes("UTF-8")))
  }

  def validate(xmlInput: File): Either[ValidationError, Unit] = {
    validate(xmlInput.toString, new FileInputStream(xmlInput))
  }

  def validate(inputName: String, xmlInput: InputStream): Either[ValidationError, Unit]
}

trait ExternalValidator extends XMLValidator {
  val programName: String
  val checkArgs = Seq("--version")

  case class ExternalValidatorError(override val msg: String)
    extends ValidationError(msg)

  def testExists(): Either[ValidationError, Unit] = {
    Try(new CheckCommandExists(programName, checkArgs)) match {
      case Success(_) => Right(())
      case Failure(t) => Left(ExternalValidatorError(s"Program does not exist: $programName"))
    }
  }
}

object XMLLintValidator {
  case class XMLLintValidatorError(override val msg: String)
    extends ValidationError(msg)
}

/**
  * validate the XML against our RELAX-NG schema
  */
class XMLLintValidator(val logXmlLintOutput: Boolean = false,
                       override val tempDirParam: Option[File] = None)
  extends ExternalValidator with ValidateUsesTempDir {
  import XMLValidator._
  import XMLLintValidator._

  override def usesTempDirErrorTypeCTOR: String => XMLLintValidatorError = XMLLintValidatorError.apply

  override val programName = "xmllint"
  val args = Seq("--noout", "--nowarning")

  implicit def errorType: String => ValidationError = XMLLintValidatorError.apply

  override val defaultTempDirPrefix = "accregexvalidator"

  private def schemaToTempFile(tempDir: File): Either[ValidationError, File] = {
    val res = getClass().getResourceAsStream(schemaResource)

    for {
      schema <- StreamReader.readStream(res)
      f <- stringToTempFile(tempDir)(schema)
    } yield f
  }


  override def validate(inputName: String, xmlInput: InputStream): Either[ValidationError, Unit] = {
    def exec(schemaLoc: File, toVerify: File): Either[ValidationError, ProgramOutput] = {
      val xmllintArgs = args ++ Seq("--relaxng", schemaLoc.toString, toVerify.toString)

      def error(s: String) = new XMLValidationError(s"Error while running $programName " + s)
      Runner.run(programName, xmllintArgs, logXmlLintOutput) match {
        case e: ExceptionOnRun => Left(error("ExceptionOnRun").withCause(e.e))
        case e: NonzeroExitCode => Left(error("NonzeroExitCode"))
        case q: ProgramOutput => Right(q)
      }
    }

    for {
      _ <- testExists()
      xmlStr <- StreamReader.readStream(xmlInput)
      tempDir <- getTempDir()
      xmlFile <- stringToTempFile(tempDir)(xmlStr)
      schema <- schemaToTempFile(tempDir)
      _ <- exec(schema, xmlFile)
    } yield {}
  }
}

/**
  * do-nothing instance of XMLValidator
  */
class SkipXMLValidator extends XMLValidator {
  override def validate(inputName: String, xmlInput: String): Either[ValidationError, Unit] = Right(())

  override def validate(xmlInput: File): Either[ValidationError, Unit] = Right(())

  override def validate(inputName: String, xmlInput: InputStream): Either[ValidationError, Unit] =
    Right(())
}


object StreamReader {

  //see https://stackoverflow.com/questions/309424/read-convert-an-inputstream-to-a-string
  def jReadStream(in: InputStream): String = {
    import java.io.InputStreamReader

    val byteArrayOutputStream = new ByteArrayOutputStream()
    val bufferSize = 1024
    val buffer = new Array[Byte](bufferSize)

    var rsz = 0

    do {
      rsz = in.read(buffer, 0, buffer.length)
      if(rsz >= 0) {
        byteArrayOutputStream.write(buffer, 0, rsz)
      }
    } while(rsz >= 0)

    new String(byteArrayOutputStream.toByteArray, "UTF-8")
  }

  val readStream: ValidateF[InputStream, String] =
    (i: InputStream, errorType: String => ValidationError) => {
      Try(jReadStream(i)) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(errorType(s"Failed to read from InputStream $i").withCause(t))
      }
    }

}
