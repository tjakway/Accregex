package com.jakway.gnucash.io.test

import java.io.{File, PrintWriter}
import java.nio.file.Files

import com.jakway.gnucash.io.XMLLintValidator
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Elem, Node, XML}

class TestXMLValidator(val regDocRoot: Node) extends FlatSpec with Matchers {
  val inputName = "TestXMLValidatorInput"

  val regDocElem = regDocRoot.asInstanceOf[Elem]

  "XMLValidator" should "support >1 validator instance simultaneously" in {
    val v1 = new XMLLintValidator()
    val v2 = new XMLLintValidator()
  }

  def toTempFile(n: Node): File = {
    val tf = Files.createTempFile(null, null).toFile
    tf.deleteOnExit()

    assert(tf.canWrite)

    XML.write(new PrintWriter(tf), n, "UTF-8", true, null)

    tf
  }

  "XMLValidator" should "validate reg_doc_example as a Node" in {
    new XMLLintValidator().validateNode(inputName, regDocRoot) shouldEqual Right(())
  }

  "XMLValidator" should "validate reg_doc_example as a file" in {
    val tf = toTempFile(regDocElem.copy())
    val res = new XMLLintValidator().validate(tf)

    tf.delete()

    res shouldEqual Right(())
  }

}
