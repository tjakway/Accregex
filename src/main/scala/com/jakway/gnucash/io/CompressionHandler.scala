package com.jakway.gnucash.io

import java.io.{File, FileInputStream, InputStream, OutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream, ZipException}

import com.jakway.gnucash.{Config, ValidatedConfig}
import com.jakway.gnucash.error.{ValidateUsesTempDir, ValidationError}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}


/*
  * Module entry point: CompressionHandler.newGZIPHandler is used to get a new instance of CompressionHandler
  */


trait CompressionHandler {
  def inputToStream(): Either[ValidationError, InputStream]
  def wrapOutputStream(os: OutputStream): Either[ValidationError, OutputStream]
}

object CompressionHandler {
  private val bufSize: Int = 16384

  class CompressionHandlerError(override val msg: String)
    extends ValidationError(msg)

  case object InputIsCompressedError
    extends CompressionHandlerError("See exception for details")

  case class WrapOutputStreamError(override val msg: String)
    extends CompressionHandlerError(msg)


  def newGZIPHandler(config: ValidatedConfig): Either[ValidationError, CompressionHandler] = {
    for {
      isCompressed <- inputIsCompressed(config.inputPath)
    } yield {
      new GzipCompressionHandler(config.inputPath,
        isCompressed, bufSize, config.verbosity)
    }
  }


  private def inputIsCompressed(inputPath: File): Either[ValidationError, Boolean] = Try {
    var stream: Option[InputStream] = None
    var res = false

    try {
      stream = Some(new GZIPInputStream(new FileInputStream(inputPath)))
      res = true
    } catch {
      case _: ZipException => {
        res = false
      }
    }

    stream.foreach(_.close())
    res
  } match {
    case Success(r) => Right(r)
    case Failure(t) => Left(
      InputIsCompressedError.withCause(t))
  }
}


private class GzipCompressionHandler(inputPath: File,
                                 inputIsCompressed: Boolean,
                                 bufSize: Int,
                                 verbosity: Config.Verbosity)
  extends CompressionHandler {
  import CompressionHandler._
  val logger: Logger = LoggerFactory.getLogger(getClass())

  def mkStream() = new FileInputStream(inputPath)


  override def inputToStream(): Either[ValidationError, InputStream] = {
    lazy val origStream = mkStream()

    Try {
      new GZIPInputStream(origStream, bufSize)

      origStream
    } recover {
      case _: ZipException => {
        if(verbosity.debug) {
          logger.debug(s"Caught ZipException, ${inputPath} is not a gzipped file")
        }

        //reopen the stream just to be sure
        origStream.close()
        mkStream()
      }
    }
  } match {
    case Success(e) => Right(e)
    case Failure(t) => Left(
      new CompressionHandlerError("Could not open inputStream, see exception for details")
        .withCause(t)
    )
  }

  override def wrapOutputStream(os: OutputStream): Either[ValidationError, OutputStream] = {
    def f(wrap: OutputStream => OutputStream, onFailure: String) = {
      Try(wrap(os)) match {
        case Success(o) => Right(o)
        case Failure(t) => Left(WrapOutputStreamError(onFailure).withCause(t))
      }
    }

    if (inputIsCompressed) {
      f(new GZIPOutputStream(_), "Error occurred while wrapping with GZIPOutputStream")
    } else {
      f(x => x, "Error occurred without wrapping with GZIPOutputStream")
    }
  }
}

