package com.jakway.gnucash.io

import java.io._
import java.nio.file.Files

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.io.XMLValidator.XMLValidationError
import com.jakway.gnucash.parser.{ValidateF, ValidationError}
import com.jakway.util.XMLUtils
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
      new XMLLintValidator(conf.debug, tempDir)
    }

    val outputValidator = if(conf.skipOutputValidation) {
      new SkipXMLValidator()
    } else {
      //if we're performing both input and output verification then
      //use the same validator object for both
      if(inputValidator.isInstanceOf[XMLLintValidator]) {
        inputValidator
      } else {
        new XMLLintValidator(conf.debug, tempDir)
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

/**
  * validate the XML against our RELAX-NG schema
  */
class XMLLintValidator(logXmlLintOutput: Boolean = false, val tempDir: Option[File] = None) extends ExternalValidator {
  import XMLValidator._

  override val programName = "xmllint"
  val args = Seq("--noout", "--nowarning")

  case class XMLLintValidator(override val msg: String)
    extends ValidationError(msg)
  implicit def errorType: String => ValidationError = XMLLintValidator.apply

  val defaultTempDirPrefix = "accregexvalidator"

  private def getTmpDir(): Either[ValidationError, File] = {

    def checkDir(errHeader: String, d: File): Either[ValidationError, File] = {
      def err = (x: String) => Left(XMLLintValidator(errHeader + ": " + x))
      //wrap IO actions in a Try to catch SecurityExceptions
      if(!d.exists() && !Try(d.mkdirs()).getOrElse(false)) {
        err(s"expected $d to exist")
      } else if(!d.isDirectory) {
        err(s"expected $d to be a directory")
      } else if(!d.canWrite && !Try(d.setWritable(true)).getOrElse(false)) {
        err(s"expected $d to be writeable")
      } else {
        Right(d)
      }
    }

    tempDir match {
      //fail if the passed dir isn't valid
      case Some(dir) => checkDir(s"Could not use passed temp dir $tempDir", dir)

      //try and generated one otherwise
      case None => {
        Try(Files.createTempDirectory(defaultTempDirPrefix)) match {
          case Success(dir) => checkDir(s"Could not use generated temp dir $dir", dir.toFile)
              .map { x => x.deleteOnExit(); x }
          case Failure(t) => Left(XMLLintValidator("Could not create temporary dir").withCause(t))
        }
      }
    }
  }

  private def getTempFile(dir: File): Either[ValidationError, File] = {
    Try(Files.createTempFile(dir.toPath, null, ".xml")) match {
      case Success(f) => Right(f.toFile)
      case Failure(t) => Left(
        XMLLintValidator(s"Could not create temp file in $dir").withCause(t))
    }
  }

  private def stringToTempFile(dir: File)(str: String): Either[ValidationError, File] = {
    import java.io.PrintWriter
    //close over the XML and write it out to the passed file
    def write(dest: File): Either[ValidationError, Unit] = {
      val res = Try(new PrintWriter(dest))
        .map { p =>
          p.println(str)
          p.close()
        }
      res match {
        case Success(_) => Right(())
        case Failure(t) => Left(
          XMLLintValidator(s"Failed to write `$str` to file $dest").withCause(t))
      }
    }

    for {
      f <- getTempFile(dir)
      _ <- write(f)
    } yield f
  }

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
      tempDir <- getTmpDir()
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
