package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser.xml.{FilterTransactionsDiff, HasDiffEngine, XMLEqual}
import com.jakway.util.XMLUtils
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node

class TestXMLDiff(val regDocXML: Node) extends FlatSpec with Matchers {

  abstract class TestXMLDiffInstance(val testName: String)
    extends FlatSpec with Matchers {
    def getEngine(originalXML: String, newXML: String): HasDiffEngine

    testName should "validate a Node against itself" in {
      for {
        str <- XMLUtils.nodeToString(regDocXML)
      } yield {
        getEngine(str, str).passes() shouldEqual Right(())
      }
    }
  }

  class TestFilterTransactionsDiff
    extends TestXMLDiffInstance(classOf[FilterTransactionsDiff].getClass().getName()) {

    override def getEngine(originalXML: String, newXML: String): HasDiffEngine =
      new FilterTransactionsDiff(originalXML,
        Set(), newXML, Set(), _ => Right(null))
  }

  class TestXMLEqualDiff
    extends TestXMLDiffInstance(classOf[XMLEqual].getClass().getName()) {

    override def getEngine(originalXML: String, newXML: String): HasDiffEngine =
      new XMLEqual(originalXML, newXML)
  }

}
