package com.jakway.util

import java.io.StringWriter

import com.jakway.gnucash.error.ValidationError
import org.w3c.dom.{Node, NodeList}

import scala.util.{Failure, Success, Try}

object XMLUtils {

  /**
    * copy the list into a seq
    * @param list
    * @return
    */
  def nodeListToSeq(list: NodeList): Seq[Node] = {
    if(list.getLength == 0) {
      Seq()
    } else {
      import scala.collection.mutable
      val buf: mutable.ArrayBuffer[Node] = new mutable.ArrayBuffer(list.getLength)

      for(i <- 0 to list.getLength) {
        buf.:+(list.item(i))
      }

      buf
    }
  }

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

      nodeListToSeq(thisNode.getChildNodes) match {
        case Seq() => nextAcc
        case childNodes@_ => childNodes.foldLeft(nextAcc)(helper)
      }
    }

    helper(Seq(), top)
  }

  def isPCData(n: Node): Boolean =
    n.getNodeType() == Node.CDATA_SECTION_NODE

  def filterPCData(c: TraversableOnce[Node]): TraversableOnce[Node] =
    c.filter(!isPCData(_))

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
