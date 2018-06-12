package com.jakway.gnucash.io

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream, ZipException}

import com.jakway.gnucash.{Config, ValidatedConfig, io}
import com.jakway.gnucash.error.{ValidateUsesTempDir, ValidationError}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}


/*
  * Module entry point: CompressionHandler.newGZIPHandler is used to get a new instance of CompressionHandler
  */


trait CompressionHandler {
  def inputToStream(): Either[ValidationError, InputStream]
  def wrapOutputStream(os: OutputStream): Either[ValidationError, OutputStream]


  def mkStream(i: File): InputStream = new BufferedInputStream(new FileInputStream(i))
}


object CompressionHandler {
  private val bufSize: Int = 16384
  private val logger: Logger = LoggerFactory.getLogger(classOf[CompressionHandler])

  class CompressionHandlerError(override val msg: String)
    extends ValidationError(msg)

  case object InputIsCompressedError
    extends CompressionHandlerError("See exception for details")

  case class WrapOutputStreamError(override val msg: String)
    extends CompressionHandlerError(msg)

  /**
    * get the compression handler appropriate for this configuration
    * @param config
    * @return
    */
  def apply(config: ValidatedConfig): Either[ValidationError, CompressionHandler] = {
    //TODO: fix compression handling, currently ignoring all compression settings
    Right(new NoCompression(config.inputPath))
  }


  private def newGZIPHandler(config: ValidatedConfig): Either[ValidationError, CompressionHandler] = {
    for {
      isCompressed <- inputIsCompressed(config.inputPath, config.verbosity)
    } yield {
      new GzipCompressionHandler(config.inputPath,
        isCompressed, config.compress, bufSize, config.verbosity)
    }
  }


  private[io] def inputIsCompressed(inputPath: File, verbosity: Config.Verbosity): Either[ValidationError, Boolean] = Try {
    var stream: Option[InputStream] = None
    var res = false

    try {
      stream = Some(new GZIPInputStream(new FileInputStream(inputPath), bufSize))
      res = true
    } catch {
      case e: ZipException => {
        if(verbosity.debug) {
          logger.debug(s"Caught ZipException for $inputPath: $e")
        }
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


private class NoCompression(val input: File) extends CompressionHandler {
  override def inputToStream(): Either[ValidationError, InputStream] =
    Try(mkStream(input)) match {
      case Success(i) => Right(i)
      case Failure(t) => Left(
        new io.CompressionHandler.CompressionHandlerError(s"Exception thrown while opening ${input}"))
    }

  override def wrapOutputStream(os: OutputStream): Either[ValidationError, OutputStream] = Right(os)
}

private class GzipCompressionHandler(inputPath: File,
                                 inputIsCompressed: Boolean,
                                 compressOutput: Boolean,
                                 bufSize: Int,
                                 verbosity: Config.Verbosity)
  extends CompressionHandler {
  import CompressionHandler._
  val logger: Logger = LoggerFactory.getLogger(getClass())


  override def inputToStream(): Either[ValidationError, InputStream] = {
    lazy val origStream = mkStream(inputPath)

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
        mkStream(inputPath)
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

    if (compressOutput) {
      f(new GZIPOutputStream(_), "Error occurred while wrapping with GZIPOutputStream")
    } else {
      f(x => x, "Error occurred without wrapping with GZIPOutputStream")
    }
  }
}

