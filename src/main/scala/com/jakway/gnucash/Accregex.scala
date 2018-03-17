package com.jakway.gnucash

import org.scalatest.fixture

import scala.annotation.tailrec
import scala.reflect.internal.Precedence
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

trait ValidationError

/**
  * TODO:
  * -check count-data attribute cd:type=="book" && text=="1"
  *
  * -
  */
object Validate {
  private def onlyOne[A](errorType: String => ValidationError)(s: Seq[A]): Either[ValidationError, A] = {
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

  private def isBook(node: Node): Boolean = {
    node.namespace == "gnc" && node.label == "book"
  }
  def apply(root: Node) = {

  }
}

object XMLUtils {
  def searchNode(f: Node => Boolean)(top: Node): Seq[Node] = {

    @tailrec
    def helper(acc: Seq[Node])(thisNode: Node): Seq[Node] = {
      val nextAcc = if(f(thisNode)) {
        acc.+:(thisNode)
      } else {
        acc
      }

      val child = thisNode.child
      if(child.isEmpty) {
        nextAcc
      } else {
        child.foldLeft(nextAcc) match {
          case (fAcc, nextNode) => helper(fAcc)(nextNode)
        }
      }
    }

    helper(Seq())(top)
  }
}