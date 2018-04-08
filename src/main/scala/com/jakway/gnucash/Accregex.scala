package com.jakway.gnucash

import com.jakway.gnucash.io.Driver


object Accregex {
  private def fatalError(msg: String): Unit = {
    System.err.println(s"Fatal error in ${Config.progName}: $msg")
    System.exit(1)
  }

  def main(args: Array[String]): Unit = {
    UnvalidatedConfig
      .parser.parse(args, UnvalidatedConfig.default)
      .map { unvalidatedConfig =>
        unvalidatedConfig.validate() match {
          case Right(validatedConfig) =>
            new Driver(validatedConfig).run()

          case Left(errMsg) => fatalError(errMsg)
        }
      }
  }
}
