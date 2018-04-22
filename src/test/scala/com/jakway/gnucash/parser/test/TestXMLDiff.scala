package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser.xml.HasDiffEngine
import com.jakway.util.XMLUtils
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node

class TestXMLDiff(val regDocXML: Node) extends FlatSpec with Matchers {

  abstract class TestXMLDiffInstance[A](val testName: String) {
    def getEngine(originalXML: String, newXML: String): HasDiffEngine

    testName should "validate a Node against itself" in {
      for {
        str <- XMLUtils.nodeToString(regDocXML)
      } yield {
        getEngine(str, str).passes() shouldEqual Right(())
      }
    }
  }

}
