package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser.{Parser, ValidationError}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}
import scala.xml.{Node, XML}

class TestParser(val regDocResource: String) extends FlatSpec with Matchers {
  val parser = new Parser

  /**
    * for errors before tests are run
    * @param msg
    */
  case class TestParserLoadError(msg: String)
    extends RuntimeException(msg)
    with ValidationError

  //val regDocRoot = XML.load(getClass.getResource(regDocResource))

  def loadBookFromResource(r: String) = {
    val xml = XML.load(getClass.getResource(regDocResource))
    Try(parser.findBookNode(xml)(TestParserLoadError.apply _).right.get) match {
      case Success(b) => b
      case Failure(t) => {
        val ex = TestParserLoadError(s"Failed to load resource $r, caused by $t")
        ex.initCause(t)
        throw ex
      }
    }
  }

  val book = loadBookFromResource(regDocResource)



  "The Parser" should "load the number of accounts" in {
    //TODO: parameterize expected value
    parser.extractNumAccounts(book) shouldEqual Right(65)
  }
}

class TestParserRegDocXML extends TestParser("/reg_doc_example.gnucash")
