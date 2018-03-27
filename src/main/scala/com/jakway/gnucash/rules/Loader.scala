package com.jakway.gnucash.rules

import com.jakway.gnucash.parser.ValidationError
import org.json4s.{JObject, JsonAST}
import org.json4s.JsonAST.{JField, JString}

import scala.util.{Failure, Success, Try}

class TransactionRuleLoaderError(override val msg: String)
  extends ValidationError(msg)

/**
  * @param srcToParse JSON source to parse--DO NOT PASS A FILE
  */
class Loader(val srcToParse: String) {

  def parsePriority(obj: JObject): Either[TransactionRuleLoaderError, Double] =
    obj.extractOpt[String] match {
          //if the user entered a priority...
        case Some(priorityStr) => {
          //try to parse it as a number
          case class PriorityParseException(override val msg: String)
            extends TransactionRuleLoaderError(msg)

          val errMsg = s"Could not parse $priorityStr as a number"

          Try(priorityStr.toDouble) match {
            case Success(a) => Right(a)
            case Failure(ex) => {
              val err = PriorityParseException(errMsg)
              err.initCause(ex)
              Left(err)
            }
          }
        }

          //otherwise use the default priority
        case None => Right(UnlinkedTransactionRule.defaultPriority)
      }

  def parseRule(obj: JObject): Either[TransactionRuleLoaderError, UnlinkedTransactionRule] = {
    val res = for {
      JField("pattern", JString(pattern)) <- obj
      JField("sourceAccount", JString(src)) <- obj
      JField("destAccount", JString(dst)) <- obj
    } yield { (pattern, sourceAccount, destAccount) }


  }

  def parse: Either[TransactionRuleLoaderError, Seq[UnlinkedTransactionRule]] = {

  }


}

