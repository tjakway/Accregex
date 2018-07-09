package com.jakway.gnucash.io

import java.io.File

import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.io.Output.WriteError
import com.jakway.gnucash.rules.RuleApplicator.RuleApplicatorLogEvent

import scala.util.{Failure, Success, Try}
import scala.xml.XML

object Output {
  case class WriteError(override val msg: String)
    extends ValidationError(msg)
}

class Output(val compressionHandler: CompressionHandler,
             val node: scala.xml.Node,
             val events: Seq[RuleApplicatorLogEvent]) {

  def write(outputPath: File, enc: String): Either[ValidationError, File] = {
    def wrap[A](t: Try[A], onError: String): Either[ValidationError, A] = t match {
      case Success(r) => Right(r)
      case Failure(t) => Left(WriteError(onError).withCause(t))
    }

    for {
      /*fos <- wrap(Try { new FileOutputStream(config.outputPath) },
        s"Error opening ${config.outputPath}")
      os <- o.compressionHandler.wrapOutputStream(fos)
      osw <- wrap(Try { new OutputStreamWriter(os, config.enc) },
        s"Error opening OutputStreamWriter around ${config.outputPath}")*/
      _ <- wrap(Try { XML.save(outputPath.toString,
        scala.xml.Utility.trim(node), enc, true, null) },
        s"Error while writing XML node to OutputStreamWriter")
    } yield {
      outputPath
    }
  }
}



