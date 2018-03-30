package com.jakway.gnucash.rules

import com.jakway.gnucash.parser.ValidationError

import scala.reflect.ClassTag

abstract class UniquePriorityOrdering[A, B](val toOrder: Seq[A]) {
  class UniquePriorityOrderingError(override val msg: String)
    extends ValidationError(msg)

  case class EmptyArgument(override val msg: String)
    extends UniquePriorityOrderingError(msg)


  case class PriorityCollision(override val msg: String)
    extends UniquePriorityOrderingError(msg)

  implicit val ordering: Ordering[B]

  /**
    * extract the priority from the parameter
    * @param obj
    * @return
    */
  def getPriority(obj: A): B

  /**
    * @return the item with the highest priority
    *         Error if >1 item has the highest priority
    */
  def getHighestPriority()(implicit tag: ClassTag[A]): Either[ValidationError, A] = {
    import scala.util.Sorting

    if(toOrder.length < 0) {
      Left(EmptyArgument("Expected argument with length >= 1 but got empty Seq"))
    } else {
      val sorted = Sorting.stableSort(toOrder, getPriority _)

      assert(sorted.length == toOrder.length)

      if(sorted.length == 1) {
        Right(sorted.head)
      } else {
        val highestPriority = getPriority(sorted.head)

        var count = 0
        sorted.foreach { x =>
          if(getPriority(x) == highestPriority) {
            count += 1
          }
        }

        if(count > 1) {
          Left(PriorityCollision(s"More than 1 item has the highest priority in $sorted (expected exactly 1)"))
        } else {
          Right(sorted.head)
        }
      }
    }
  }
}

/**
  * Lowest number = highest priority
  */
abstract class ZeroHighPriority[A](override val toOrder: Seq[A])
  extends UniquePriorityOrdering[A, Double](toOrder) {

  override implicit val ordering: Ordering[Double] = new Ordering[Double] {
    //the opposite of what you would normally think
    //since 0 priority = highest
    override def compare(x: Double, y: Double): Int = {
      val xHigher = -1
      val xLower = 1
      val equal = 0

      if(x < y) {
        xHigher
      } else if(x > y) {
        xLower
      } else {
        equal
      }
    }
  }
}
