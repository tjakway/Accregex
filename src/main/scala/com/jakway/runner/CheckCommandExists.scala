package com.jakway.util.runner

import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

case class CommandNotFoundException(val commandName: String)
  extends RuntimeException(s"Could not find command `$commandName` on $$PATH")

object CheckCommandExists {
  def runOrThrow(logger: Logger)(commandName: String, args: Seq[String]): Try[Unit] = {
    Runner.run(commandName, args).toEither match {
      case Left(e: ExceptionOnRun) => {
        logger.warn(s"Error while executing command `$commandName $args`," +
          s"stdout: ${e.stdout}, stderr: ${e.stderr}")
        throw CommandNotFoundException(commandName)
      }
      case Right(_) => Try({})
    }
  }
}

/**
  * ***WARNING: this will run the passed command!***
  * if this isn't OK, use alternative methods like manually searching $PATH
  * @param commandName
  */
class CheckCommandExists(val commandName: String, val args: Seq[String] = Seq()) {
  private val logger: Logger = LoggerFactory.getLogger(getClass())
  private def runOrThrow = CheckCommandExists.runOrThrow(logger) _

  //if any of the following succeed, the program exists
  runOrThrow(commandName, args)
    .recover {
      case _: Throwable => runOrThrow(commandName, Seq())
    }
    .recover {
      case _: Throwable => runOrThrow(commandName, Seq("-help"))
    }
    .recover {
      case _: Throwable => runOrThrow(commandName, Seq("--help"))
    }
    .recover {
      case _: Throwable => runOrThrow(commandName, Seq("--version"))
    }.get
}