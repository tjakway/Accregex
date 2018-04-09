package com.jakway.gnucash.io

import java.io.{File, StringReader, StringWriter}

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.io.XMLValidator.{GnucashInitializeValidatorError, XMLValidationError}
import com.jakway.gnucash.parser.ValidationError
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
  def validate(inputName: String, node: scala.xml.Node, enc: String = "UTF-8"): Either[ValidationError, Unit] = {
    Try {
      //need to render the node as a string then pass it on as a StreamSource
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

/**
  * validate the XML against our RELAX-NG schema
  */
class GnucashXMLValidator extends XMLValidator {
  import XMLValidator._

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
