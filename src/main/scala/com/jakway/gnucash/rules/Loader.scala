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
  implicit def errorType: String => ValidationError = new TransactionRuleLoaderError(_)

  def parsePriority(obj: JObject): Either[ValidationError, Double] = {
    import org.json4s.Formats
    import org.json4s.DefaultFormats

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
  }

  /**
    * get a key from the map and convert it to the indicated type
    * @param m
    * @param key
    * @tparam K
    * @tparam V
    * @tparam A
    * @return
    */
  def getKey[K, V, A](m: Map[K, V])(key: K): Either[ValidationError, A] = {
    val extractedValue = m.get(key) match {
      case Some(v) => Right(v)
      case None => Left(errorType(s"Could not find key $key"))
    }

    extractedValue.flatMap(v => Try(v.asInstanceOf[A]) match {
      case Success(q) => Right(q)
      case Failure(ex) => {
        Left(errorType(s"Could not convert $extractedValue to an instance of ${classOf[A].getName}," +
          s" exception thrown: $ex"))
      }
    })
  }

  def parseRule(obj: JObject): Either[ValidationError, UnlinkedTransactionRule] = {

    def extract[A](key: String) = getKey[String, Any, A](obj.values)

    for {
      pattern <- extract[String]("pattern")
      sourceAccount <- extract[String]("sourceAccount")
      destAccount <- extract[String]("destAccount")
      priority <- parsePriority(obj)
    } yield {
      UnlinkedTransactionRule(pattern, priority.toString, sourceAccount, destAccount)
    }
  }

  def parse: Either[TransactionRuleLoaderError, Seq[UnlinkedTransactionRule]] = {
    import org.json4s._
    import org.json4s.native.JsonMethods._

    val json = parse(srcToParse)

    type AccumulatorType = Either[TransactionRuleLoaderError, Seq[UnlinkedTransactionRule]]
    val empty: Either[TransactionRuleLoaderError, Seq[UnlinkedTransactionRule]] =
      Right(Seq())

    json.children.foldLeft(empty)(c => c match {
      case (m: AccumulatorType, _) => m
      case (Right(acc), obj@JObject(_)) => parseRule(obj) match {
        case Left(r) => r
        case Right(q) => Right(acc.+:(q))
      }
      case (_, x) => Left(errorType(s"Expected object child of $json but got $x")): AccumulatorType
    })
  }


}

