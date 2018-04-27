package com.jakway.util

import java.io.StringWriter

import com.jakway.gnucash.error.ValidationError

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, PCData}

object XMLUtils {
  def searchNode(f: Node => Boolean)(top: Node): Seq[Node] = {

    /**
      * TODO: make sure this doesn't consume too much stack space...
      * @param acc
      * @param thisNode
      * @return
      */
    def helper(acc: Seq[Node], thisNode: Node): Seq[Node] = {
      val nextAcc = if(f(thisNode)) {
        acc.+:(thisNode)
      } else {
        acc
      }

      thisNode.child match {
        case Seq() => nextAcc
        case childNodes@_ => childNodes.foldLeft(nextAcc)(helper)
      }
    }

    helper(Seq(), top)
  }

  def filterPCData(c: TraversableOnce[Node]): TraversableOnce[Node] =
    c.filter(n => !(n.isInstanceOf[PCData] || n.label == "#PCDATA"))

  def nodeToString(node: Node): Either[ValidationError, String] = {
    case class NodeToStringError(override val msg: String)
      extends ValidationError(msg)

    Try {
      //need to render the node as a string then pass it on as a StreamSource
      val enc = "UTF-8"
      val sw: StringWriter = new StringWriter()
      scala.xml.XML.write(sw, node, enc,
        true, null) //null means no doctype

      sw.toString()
    } match {
      case Success(xmlStr) => Right(xmlStr)
      case Failure(t) => Left(new NodeToStringError(
        s"Error rendering a scala XML node ($node) to a String")
        .withCause(t))

    }
  }
}
