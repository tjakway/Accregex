package com.jakway.gnucash

import scala.xml.{Node, XML}

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
  def apply(root: Node) = {

  }
}