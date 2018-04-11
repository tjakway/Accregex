package com.jakway.util.runner

import java.io.{ByteArrayInputStream, File, IOException, InputStream => JInputStream}

import org.slf4j.{Logger, LoggerFactory}

import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}


object RunOutput {

  /**
    * @param runOutputSubclass
    * @param expectedExitCodeMsg a string describing the expected exit code value or range of values
    * @param actualExitCode
    * @tparam A
    */
  case class RunnerImplementationException[A](runOutputSubclass: Class[A],
                                              expectedExitCodeMsg:  String,
                                              actualExitCode:    Int)
    extends RuntimeException(s"Error in Runner implementation: subclass ${runOutputSubclass.getCanonicalName}" +
      s" should only be instantiated with exit code $expectedExitCodeMsg but had actual exit code $actualExitCode")

  def assertExitCode[A](thisClass: Class[A], check: Int => Boolean, expectedMsg: String, actual: Int) = {
    if(!check(actual)) {
      throw RunnerImplementationException(thisClass, expectedMsg, actual)
    }
  }
}

case class RunnerException(msg: String, t: Option[Throwable] = None)
  extends RuntimeException(msg)
{
  //call initCause if we were passed a Throwable
  t match {
    case Some(t) => initCause(t)
    case None => {}
  }
}

sealed trait RunOutput {
  val progName: String
  val args: Seq[String]
  val stdout: String
  val stderr: String

  //equivalent to toTry.get()
  def get(): ProgramOutput = toEither match {
    case Right(x) => x
    case Left(e) => {
      e.throwThis()
      //dummy value--we just threw an exception
      null
    }
  }

  def toTry: Try[ProgramOutput] = Try(get())

  def toEither: Either[ExceptionOnRun, ProgramOutput] = this match {
    case p: ProgramOutput => Right(p)
    case e: ExceptionOnRun => Left(e)
  }

  def throwThis(): Unit

  protected def throwSelf(e: Option[Exception]): Unit =
    throw RunnerException(s"Error while running $progName with args" +
      s" $args\nstdout: $stdout\nstderr: $stderr", e)
}

/**
  * superclass for non-exceptional output
  */
abstract class ProgramOutput(override val progName: String, override val args: Seq[String],
                             override val stdout: String, override val stderr: String,
                             val exitCode: Int) extends RunOutput
{
  //force subclasses to override the relevant parameters of the exit code check
  def check: Int => Boolean
  def getExpectedMsg(): String

  RunOutput.assertExitCode(getClass(), check, getExpectedMsg(), exitCode)

  override def throwThis(): Unit = throwSelf(None)
}

case class NonzeroExitCode(override val progName: String, override val args: Seq[String],
                       override val stdout: String, override val stderr: String,
                       override val exitCode: Int)
  extends ProgramOutput(progName, args, stdout, stderr, exitCode)
{
  override def check = _ != 0
  override def getExpectedMsg() = "nonzero exit code"
}

//used to represent success
case class ZeroExitCode(override val progName: String, override val args: Seq[String],
                        override val stdout: String, override val stderr: String,
                        override val exitCode: Int)
  extends ProgramOutput(progName, args, stdout, stderr, exitCode)
{

  override def check = _ == 0
  override def getExpectedMsg() = "zero exit code"
}

//exceptional subclasses

class ExceptionOnRun(override val progName: String, override val args: Seq[String],
                     override val stdout: String, override val stderr: String,
                     val e: Exception)
  extends RunOutput {
  override def throwThis(): Unit = throwSelf(Some(e))
}

case class IOExceptionOnRun(override val progName: String, override val args: Seq[String],
                       override val stdout: String, override val stderr: String,
                       override val e: IOException)
  extends ExceptionOnRun(progName, args, stdout, stderr, e)




object Runner {

  val logger: Logger = LoggerFactory.getLogger(getClass())

  val emptyInputStream: JInputStream = new ByteArrayInputStream(Array.empty[Byte])

  //TODO: add cwd to RunOutput & subclasses
  //we have it, might as well include it
  def run(progName: String, args: Seq[String], logOutput: Boolean = false, cwd: Option[File] = None,
          stdin: JInputStream = emptyInputStream): RunOutput
    = {
    var stdout = ""
    var stderr = ""

    def appendStdout(line: String): Unit = {
      stdout = stdout + line + "\n"
    }
    def appendStderr(line: String): Unit = {
      stderr = stderr + line + "\n"
    }

    //make it lazy to guarantee no exceptions before this point
    lazy val processLogger = ProcessLogger(appendStdout, appendStderr)

    //only the final parameter of the subclasses of RunOutput varies--
    //either an exception or an exit code
    val mkRunOutput: (String, Seq[String], String, String) => RunOutput = Try {

      //run the program
      //don't connect stdin
      Process(Seq(progName) ++ args, cwd)
        //connect stdin
        .#<(stdin)
        .run(processLogger, false)
        //block until it returns
        .exitValue()
    } match {
      case Failure(e: IOException) => IOExceptionOnRun(_, _, _, _, e)
      case Failure(e: Exception) =>   new ExceptionOnRun(_, _, _, _, e)
      case Success(exitCode)
          if exitCode == 0       =>   ZeroExitCode(_, _, _, _, exitCode)
      case Success(exitCode)
          if exitCode != 0       =>   NonzeroExitCode(_, _, _, _, exitCode)
    }

    if(logOutput) {
      logger.debug(s"Ran $progName with args $args, stdout: $stdout, stderr: $stderr")
    }
    mkRunOutput(progName, args, stdout, stderr)
  }
}
