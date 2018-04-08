package com.jakway.gnucash

import java.io.File

object Config {
  val progName = "accregex"
}

import Config._

case class UnvalidatedConfig(inputPath: String,
                             rulesPath: String,
                             outputPath: Option[String],
                             summarize: Boolean,
                             compress: Boolean) {

  def validate(): Either[String, ValidatedConfig] = {
    for {
      input <- checkGnucashInputFile(new File(inputPath))
      out <- getOutputFile(input, outputPath)
      rules <-checkRuleInputFile(new File(rulesPath))
    } yield {
      ValidatedConfig(input, rules, out, summarize, compress)
    }
  }

  def getOutputFile(inputFile: File, outputPath: Option[String]): Either[String, File] = outputPath match {
    case Some(explicitFile) => checkOutputFile(new File(explicitFile))
    case None => calculateOutputFile(inputFile)
  }

  def validateOrExit(): ValidatedConfig = {
    validate() match {
      case Right(x) => x
      case Left(errMsg) => {
        System.err.println(errMsg)
        System.exit(1)
        null: ValidatedConfig
      }
    }
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
        Left("Could not generate an output filename, please pass one explicitly")
      } else {
        Right(candidate)
      }

    } else {
      Right(firstTry)
    }
  }


  private def iterateChecks[A](checks: Seq[(Boolean, String)])(onSuccess: A): Either[String, A] = {
    checks.find(_._1).map(_._2) match {
      case Some(errMsg) => Left(errMsg)
      case None => Right(onSuccess)
    }
  }

  def checkOutputFile(outFile: File): Either[String, File] = {
    iterateChecks(Seq(
      (outFile.exists(), s"$outFile already exists"),
      (!outFile.canWrite(), s"cannot write to $outFile")))(outFile)
  }

  def checkRuleInputFile(rules: File): Either[String, File] = {
    iterateChecks(Seq(
      (!rules.exists(), s"${rules} does not exist"),
      (!rules.canRead(), s"$rules exists but cannot be read"),
      (rules.isDirectory(), s"$rules is a directory")
    ))(rules)
  }

  def checkGnucashInputFile(gnucashInput: File): Either[String, File] =
    checkRuleInputFile(gnucashInput)
}

case class ValidatedConfig(inputPath: File,
                           rulesPath: File,
                           outputPath: File,
                           summarize: Boolean,
                           compress: Boolean)

object UnvalidatedConfig {
  val default: UnvalidatedConfig =
    UnvalidatedConfig("", "rules.conf", None, true, true)

  val parser = new scopt.OptionParser[UnvalidatedConfig](progName) {
    head(progName)

    opt[String]('f', "rules")
      .action((x, c) => c.copy(rulesPath = x))
      .text(s"The file to load transaction rules from (default=${default.rulesPath})")

    opt[Boolean]('s', "summarize")
      .action((x, c) => c.copy(summarize = x))
      //TODO
      .text(s"(NOT IMPLEMENTED) Whether to print a summary of changes (default=${default.summarize})")

    opt[String]('i', "input")
      .action((x, c) => c.copy(inputPath = x))
      .text("The GNUCash file to process")
      .required()

    opt[String]('o', "output")
      .action((x, c) => c.copy(outputPath = Some(x)))
      .text("The destination file (default=input_filename.out")

    opt[Boolean]('c', "compress-output")
      .action((x, c) => c.copy(compress = x))
      //TODO
      .text(s"(NOT IMPLEMENTED) Whether to compress the output file (default=${default.compress}")
  }
}
