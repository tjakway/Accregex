package com.jakway.gnucash.io

import java.io.{File, StringReader, StringWriter}
import java.nio.file.Files

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.io.XMLValidator.{GnucashInitializeValidatorError, XMLValidationError}
import com.jakway.gnucash.parser.{ValidateF, ValidationError}
import com.jakway.util.runner.CheckCommandExists
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{Schema, SchemaFactory}

import scala.sys.process.processInternal.InputStream
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
  def validate(inputName: String, node: scala.xml.Node): Either[ValidationError, Unit] = {
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
    validate(inputName, new StreamSource(new StringReader(xmlInput)))
  }

  def validate(xmlInput: File): Either[ValidationError, Unit] = {
    validate(xmlInput.toString, new StreamSource(xmlInput))
  }

  def validate(inputName: String, xmlInput: StreamSource): Either[ValidationError, Unit]
}

trait ExternalValidator extends XMLValidator {
  val programName: String

  case class ExternalValidatorError(override val msg: String)
    extends ValidationError(msg)

  def testExists(testCmd: String): Either[ValidationError, Unit] = {
    Try(new CheckCommandExists(programName, Seq("--version"))) match {
      case Success(_) => Right()
      case Failure(t) => Left(ExternalValidatorError(s"Program does not exist: $testCmd"))
    }
  }
}

/**
  * validate the XML against our RELAX-NG schema
  */
class XMLLintValidator(val tempDir: Option[File] = None) extends ExternalValidator {
  import XMLValidator._

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

  private def xmlToTmpFile(dir: File)(xml: String): Either[ValidationError, File] = {
    import java.io.PrintWriter
    //close over the XML and write it out to the passed file
    def write(dest: File): Either[ValidationError, Unit] = {
      val res = Try(new PrintWriter(dest))
        .foreach { p =>
          p.println(xml)
          p.close()
        }
      res match {
        case Success(_) => Right()
        case Failure(t) => Left(
          XMLLintValidator(s"Failed to write XML `$xml` to file $dest").withCause(t))
      }
    }

    for {
      f <- getTempFile(dir)
      _ <- write(f)
    } yield f
  }


  override def validate(inputName: String, xmlInput: InputStream): Either[ValidationError, Unit] = {
    for {
      xmlStr <- StreamReader.readStream(xmlInput)
      tempDir <- getTmpDir()
      file <- xmlToTmpFile(tempDir)(xmlStr)
    }
  }

  private lazy val validator: Either[ValidationError, javax.xml.validation.Validator] = {
    //read the schema resource and set up the necessary JAX machinery
    val res = for {
      fac <- Try(SchemaFactory.newInstance(XMLConstants.RELAXNG_NS_URI))
      schemaRes <- Try(getClass.getResourceAsStream(schemaResource))
      stream <- Try(new StreamSource(schemaRes))
      schema <- Try(fac.newSchema(stream))
      v <- Try(schema.newValidator())
    } yield (v)

    res match {
      case Success(x) => Right(x)
      case Failure(t) => Left(new GnucashInitializeValidatorError().withCause(t))
    }
  }

  override def validate(inputName: String, xmlInput: StreamSource): Either[ValidationError, Unit] = {
    validator.flatMap { v =>
      try {
        Right(v.validate(xmlInput))
      } catch {
        //failed because the document wasn't an instance of the schema
        case e: org.xml.sax.SAXException => Left(GnucashValidationError(inputName,
          schemaName).withCause(e))

          //failed for some other reason
        case t: Throwable => Left(new XMLValidationError("Validation failed for unknown reasons, " +
          "see exception for details").withCause(t))
      }
    }
  }
}

/**
  * do-nothing instance of XMLValidator
  */
class SkipXMLValidator extends XMLValidator {
  override def validate(inputName: String, xmlInput: String): Either[ValidationError, Unit] = Right()

  override def validate(xmlInput: File): Either[ValidationError, Unit] = Right()

  override def validate(inputName: String, xmlInput: StreamSource): Either[ValidationError, Unit] =
    Right()
}


object StreamReader {

  //see https://stackoverflow.com/questions/309424/read-convert-an-inputstream-to-a-string
  def jReadStream(inputStream: InputStream): String = {
    import java.io.InputStreamReader

    val bufferSize = 1024
    val buffer = new Array[Char](bufferSize)
    val out = new StringBuilder
    val in = new InputStreamReader(inputStream, "UTF-8")

    var rsz = 0

    do {
      rsz = in.read(buffer, 0, buffer.length)
      if(rsz >= 0) {
        out.append(buffer, 0, rsz)
      }
    } while(rsz >= 0)

    out.toString()
  }

  val readStream: ValidateF[InputStream, String] =
    (i: InputStream, errorType: String => ValidationError) => {
      Try(jReadStream(i)) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(errorType(s"Failed to read from InputStream $i").withCause(t))
      }
    }

}
