package com.jakway.gnucash


import scala.xml.{Node, NodeSeq, XML}

object Accregex {
  def main(args: Array[String]): Unit = {
    MatchTrans.test()
  }
}

object MatchTrans
{
  def test(): Unit = {
    val loc = "/home/thomas/Downloads/all_finances.xml"
    val node = XML.load(loc)

    node match {
      case <gnc:transaction>{values}</gnc:transaction> => {
        true
      }
      case _ => false
    }

  }
}

trait ValidationError

/**
  * boolean functions to run on XML nodes
  *
  */
object NodeTests {

  private def onlyOne[A](s: Seq[A])(implicit errorType: String => ValidationError): Either[ValidationError, A] = {
    val msg = "expected only 1 item in seq but got: "
    if(s.length == 1) {
      Right(s.head)
    } else if(s.isEmpty) {
      Left(errorType(msg + "empty seq"))
    }
    else {
      Left(errorType(msg + s.toString))
    }
  }

  private def hasSubNodes(root: Node, name: String)
                         (implicit errorType: String => ValidationError): Either[ValidationError, NodeSeq] = {

    val sub = (root \ name)
    sub.isEmpty match {
      case true => Left(errorType(s"could not find subnode of $root named $name"))
      case false => Right(sub)
    }
  }
}

/**
  * TODO:
  * -check count-data attribute cd:type=="book" && text=="1"
  *
  * -
  */
object Validate {

  private def isBook(node: Node): Boolean = {
    node.namespace == "gnc" && node.label == "book"
  }


  def extractNumAccounts(book: Node): Either[ValidationError, Int] = {
    case class ExtractNumAccountsError(msg: String) extends ValidationError

    ???
  }

  def apply(root: Node) = {

  }
}

