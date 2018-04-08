package com.jakway.gnucash

import java.io.File
import java.nio.file.Files

import scala.util.Try

case class UnvalidatedConfig(inputPath: String,
                             rulesPath: String,
                             outputPath: String,
                             summarize: Boolean,
                             compress: Boolean) {
  def validate(): Either[String, ValidatedConfig] = {

  }

  def calculateOutputFile(inputFile: File, maxTries: Long = 100000): Either[String, File] = {
    val outputDir = inputFile.getParentFile()

    val firstTry = new File(inputFile.toString + ".out")

    if(firstTry.exists()) {

      //iterate through the template of inputFile.out.XXXX until
      //we find a file that exists

      var i: Long = 0
      def newCandidate(x: Long) = new File(firstTry + "." + x.toString)
      var candidate: File = newCandidate(i)
      var tooManyAttempts: Boolean = false

      while(!tooManyAttempts && candidate.exists()) {
        i += 1

        if(i >= maxTries) {
          tooManyAttempts = true
        }
        else {
          candidate = newCandidate(i)
        }
      }

      if(tooManyAttempts) {
        Left("Could not generate an output file, please pass one explicitly")
      } else {
        Right(candidate)
      }

    } else {
      Right(firstTry)
    }
  }

  def checkOutputFile(outFile: File): Either[String, File] = {
    val toCheck = Seq(
      (outFile.exists(), s"$outFile already exists"),
      (!outFile.canWrite(), s"cannot write to $outFile"))

    toCheck.find(_._1).map(_._2) match {
      case Some(errMsg) => Left(errMsg)
      case None => Right(outFile)
    }
  }
}

case class ValidatedConfig(inputPath: File,
                           rulesPath: File,
                           outputPath: File,
                           summarize: Boolean,
                           compress: Boolean)

object UnvalidatedConfig {
  val default: UnvalidatedConfig =
    UnvalidatedConfig("", "rules.conf", "", true, true)

  val progName = "accregex"

  val parser = new scopt.OptionParser[UnvalidatedConfig](progName) {
    head(progName)

    opt[String]('f', "rules")
      .action((x, c) => c.copy(rulesPath = x))
      .text(s"The file to load transaction rules from (default=${default.rulesPath})")

    opt[Boolean]('s', "summarize")
      .action((x, c) => c.copy(summarize = x))
      .text(s"Whether to print a summary of changes (default=${default.summarize})")

    opt[String]('i', "input")
      .action((x, c) => c.copy(inputPath = x))
      .text("The GNUCash file to process")
      .required()

    opt[String]('o', "output")
      .action((x, c) => c.copy(outputPath = x))
      .text("The destination file (default=processed.out")

    opt[Boolean]('c', "compress-output")
      .action((x, c) => c.copy(compress = x))
      .text(s"Whether to compress the output file (default=${default.compress}")
  }
}
