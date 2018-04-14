package com.jakway.gnucash.io

import java.io._
import java.nio.file.Files

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.io.XMLValidator.{GnucashInitializeValidatorError, XMLValidationError}
import com.jakway.gnucash.parser.{ValidateF, ValidationError}
import com.jakway.util.runner._
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{Schema, SchemaFactory}

import scala.util.{Failure, Success, Try}

class CmpXML(val left: File, val right: File) {

}

object XMLValidator {
  class XMLValidationError(override val msg: String)
    extends ValidationError(msg)

  class GnucashInitializeValidatorError
    extends ValidationError("An error occurred while " +
      s"initializing the XML validator from $schemaName, see exception for details")

  case class GnucashValidationError(inputFilename: String,
                                    schemaFilename: String)
    extends ValidationError(s"Failed to validate XML document $inputFilename against " +
      s"RELAX-NG schema $schemaFilename")

  val schemaName = "gnucash-v2.rnc"
  val schemaResource = "/" + schemaName
}

trait XMLValidator {
  def validateNode(inputName: String, node: scala.xml.Node): Either[ValidationError, Unit] = {
    Try {
      //need to render the node as a string then pass it on as a StreamSource
      val enc = "UTF-8"
      val sw: StringWriter = new StringWriter()
      scala.xml.XML.write(sw, node, enc,
        true, null) //null means no doctype

      sw.toString()
    } match {
      case Success(xmlStr) => {
        validate(inputName, xmlStr)
      }
      case Failure(t) => Left(new XMLValidationError("Error rendering a scala XML node to a String")
        .withCause(t))

    }
  }

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

  case class ExternalValidatorError(override val msg: String)
    extends ValidationError(msg)

  def testExists(testCmd: String): Either[ValidationError, Unit] = {
    Try(new CheckCommandExists(programName, Seq("--version"))) match {
      case Success(_) => Right(())
      case Failure(t) => Left(ExternalValidatorError(s"Program does not exist: $testCmd"))
    }
  }
}

/**
  * validate the XML against our RELAX-NG schema
  */
class XMLLintValidator(val tempDir: Option[File] = None) extends ExternalValidator {
  import XMLValidator._

  override val programName = "xmllint"
  val checkArgs = Seq("--version")

  case class XMLLintValidator(override val msg: String)
    extends ValidationError(msg)
  implicit def errorType: String => ValidationError = XMLLintValidator.apply

  val defaultTmpDirPrefix = "accregexvalidator"

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
        Try(Files.createTempDirectory(defaultTmpDirPrefix)) match {
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
    def findProgram(): Either[ValidationError, Unit] = Try(new CheckCommandExists(programName, checkArgs)) match {
      case Success(_) => Right(())
      case Failure(t) => Left(XMLLintValidator(s"Could not find the program $programName," +
        s" perhaps you need to install it").withCause(t))
    }

    def exec(schemaLoc: File, toVerify: File): Either[ValidationError, ProgramOutput] =
      Runner.run(programName, Seq("--relaxng", schemaLoc.toString, toVerify.toString)) match {
        case q: ZeroExitCode => Right(q)
        case e: ExceptionOnRun => Left(XMLLintValidator(
          s"Error while running $programName: $e").withCause(e.e))
      }

    for {
      _ <- findProgram()
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
  def jReadStream(inputStream: InputStream): String = {
    import java.io.InputStreamReader

    val byteArrayOutputStream = new ByteArrayOutputStream()
    val bufferSize = 1024
    val buffer = new Array[Char](bufferSize)
    val in = new InputStreamReader(inputStream, "UTF-8")

    var rsz = 0

    do {
      rsz = in.read(buffer, 0, buffer.length)
      if(rsz >= 0) {
        byteArrayOutputStream.write(buffer.asInstanceOf[Array[Byte]], 0, rsz)
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
