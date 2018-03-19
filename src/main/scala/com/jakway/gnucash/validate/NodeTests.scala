package com.jakway.gnucash.validate

import scala.util.{Failure, Success, Try}
import scala.xml.{Attribute, Node, NodeSeq}

object NodeTests {
  import ValidationError._

  val namespaceSeparator = ":"

  def onlyOne[A]: ValidateF[Seq[A], A] =
    (s: Seq[A], errorType: String => ValidationError) => {

    val msg = "expected only 1 item in seq but got: "
    if (s.length == 1) {
      Right(s.head)
    } else if (s.isEmpty) {
      Left(errorType(msg + "empty seq"))
    }
    else {
      Left(errorType(msg + s.toString))
    }
  }

  def hasSubNodes: ValidateF[(Node, String), NodeSeq] =
    //scala lacks automatic tuple unboxing so we have to do it manually
    //it also doesn't have variadic type parameters which would obviate the
    //need for this
    (t: (Node, String), errorType: String => ValidationError) => {

    val (root: Node, name: String) = t

    val sub = (root \ name)
    sub.isEmpty match {
      case true => Left(errorType(s"could not find subnode of $root named $name"))
      case false => Right(sub)
    }
  }

  def hasSubNode: ValidateF[(Node, String), Node] =
    (t: (Node, String), errorType: String => ValidationError) => {

    for {
      h <- hasSubNodes(t)
      r <- onlyOne(h)
    } yield {
      r
    }
  }

  /**
    * @param s
    * @param sep
    * @param errorType
    * @return
    */
  private def splitStringOnLastIndexOf(s: String, sep: String = namespaceSeparator)
                                      (implicit errorType: String => ValidationError):
    Either[ValidationError, (String, String)] = {

    val splitIndex = s.lastIndexOf(sep)
    val prefix = s.substring(0, splitIndex)
    if(splitIndex >= (s.length - 1)) {
      Left(errorType("You cannot search for an attribute with a namespace and no name"))
    } else {
      val suffix = s.substring(splitIndex + 1, s.length - 1)
      Right((prefix, suffix))
    }
  }

  def hasAttribute(root: Node, name: String)
                        (implicit errorType: String => ValidationError): Either[ValidationError, Attribute] = {

    val noAttrError = Left(errorType(s"$root does not contain attribute $name"))

    def convResult(x: Option[Seq[Node]]): Either[ValidationError, Attribute] = x match {
      case None => noAttrError
      case Some(xs) if xs.isEmpty => noAttrError
      case Some(xs) => for {
        r <- onlyOne(xs)

        //convert the possible ClassCastException into an Either
        asAttr <- Try(r.asInstanceOf[Attribute]) match {
          case Success(q) => Right(q)
          case Failure(q) => Left(errorType(getMsg((q))))
        }
      } yield {
        asAttr
      }
    }

    //find the attribute, abstracted over whether it's prefixed
    val searchRes = name.contains(namespaceSeparator) match {
      case true => {
        for {
          (ns: String, attrName: String) <- splitStringOnLastIndexOf(name)
        } yield {
          root.attribute(ns, attrName)
        }
      }
      case false => {
        Right(root.attribute(name))
      }
    }

    searchRes.flatMap(convResult)
  }

  def findOnlyOne(f: Node => Boolean)(n: Node)
      (implicit errorType: String => ValidationError): Either[ValidationError, Node] = {

  }
}
