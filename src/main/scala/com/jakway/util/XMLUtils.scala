package com.jakway.util

import scala.annotation.tailrec
import scala.xml.Node

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
