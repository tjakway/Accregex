package com.jakway.util

import scala.annotation.tailrec
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
}
