package com.jakway.gnucash.rules

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.ObjectMapper
import com.jakway.gnucash.Config
import com.jakway.gnucash.parser.ValidationError
import org.json4s.JObject
import org.json4s.JsonAST.JValue

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
    implicit val formats = DefaultFormats

    (obj \ "priority").extractOpt[String] match {
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
        Left(errorType(s"Could not convert $extractedValue to an instance of the specified type," +
          s" exception thrown: $ex"))
      }
    })
  }

  def parseRule(obj: JObject): Either[ValidationError, UnlinkedTransactionRule] = {

    def extract[A](key: String) = getKey[String, Any, A](obj.values)(key)

    for {
      pattern <- extract[String]("pattern")
      sourceAccount <- extract[String]("sourceAccount")
      destAccount <- extract[String]("destAccount")
      priority <- parsePriority(obj)
    } yield {
      UnlinkedTransactionRule(pattern, priority.toString, sourceAccount, destAccount)
    }
  }

  /**
    * json parser with custom options to allow parsing comments
    * (need to use Jackson for this)
    * @return
    */
  private def jsonParser: String => Option[JValue] = {
    object ExtJsonParser extends org.json4s.jackson.JsonMethods {
      override def mapper: ObjectMapper = {
        //see https://github.com/codahale/jerkson/blob/master/src/main/scala/com/codahale/jerkson/Json.scala#L18
        //and https://github.com/json4s/json4s/issues/15
        val m = super.mapper
          .enable(JsonParser.Feature.ALLOW_COMMENTS)
          .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)
          .enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
          .enable(JsonParser.Feature.AUTO_CLOSE_SOURCE)

        assert(m.isEnabled(JsonParser.Feature.ALLOW_COMMENTS))
        assert(m.isEnabled(JsonParser.Feature.ALLOW_YAML_COMMENTS))
        assert(m.isEnabled(JsonParser.Feature.STRICT_DUPLICATE_DETECTION))
        assert(m.isEnabled(JsonParser.Feature.AUTO_CLOSE_SOURCE))

        m
      }
    }

    ExtJsonParser.parseOpt(_, false, false)
  }

  def parse: Either[Seq[ValidationError], Seq[UnlinkedTransactionRule]] = {
    import org.json4s._

    val jsonOpt = jsonParser(srcToParse)

    type AccumulatorType = Either[ValidationError, Seq[UnlinkedTransactionRule]]
    val empty: Either[ValidationError, Seq[UnlinkedTransactionRule]] =
      Right(Seq())


    jsonOpt match {
      case None => {
        case class JsonParseError(override val msg: String)
          extends TransactionRuleLoaderError(msg)

        Left(Seq(JsonParseError(s"Could not parse $srcToParse as JSON")))
      }
      case Some(json) => {

        def accF(a: AccumulatorType, b: JValue): AccumulatorType = (a, b) match {
          case (Right(acc), obj@JObject(_)) => parseRule(obj) match {
            case Left(r) => Left(r)
            case Right(q) => Right(acc.+:(q))
          }
          case (Left(q), _) => Left(q)
          case (_, x) => Left(errorType(s"Expected object child of $json but got $x")): AccumulatorType
        }

        ValidationError.accumulateEithers(json.children.map(_.children.foldLeft(empty)(accF)))
      }
    }

  }


}


object Loader {
  //root object is "accregex"
  val jsonRoot = Config.progName

  def loadFromFile(path: String): Either[Seq[ValidationError], Seq[UnlinkedTransactionRule]] = {
    case class TransactionLoaderFileError(override val msg: String)
      extends ValidationError(msg)

    new Loader(path).parse
  }

  def loadFromFile(path: java.io.File): Either[Seq[ValidationError], Seq[UnlinkedTransactionRule]] =
    loadFromFile(path.toString)

}
