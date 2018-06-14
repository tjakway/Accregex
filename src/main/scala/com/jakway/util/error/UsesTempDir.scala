package com.jakway.util.error

import java.io.File
import java.nio.file.Files

import scala.util.{Failure, Success, Try}

object UsesTempDir {

  /**
    * get or create a temp dir usable by the passed object
    * @param u
    * @tparam E
    * @return
    */
  def mkTempDir[E <: WithCause[E]](u: UsesTempDir[E]): Either[E, File] = {
    u.getTempDir()
  }
}

trait UsesTempDir[E <: WithCause[E]] {
  /**
    * a user-specified temporary dir that will be checked first
    * set to None if user input is not desirable
    */
  val tempDirParam: Option[File]

  val defaultTempDirPrefix: String
  val defaultSuffix = ".xml"

  def usesTempDirErrorTypeCTOR: String => E

  def getTempDir(): Either[E, File] = {

    def checkDir(errHeader: String, d: File): Either[E, File] = {
      def err = (x: String) => Left(usesTempDirErrorTypeCTOR(errHeader + ": " + x))
      //wrap IO actions in a Try to catch SecurityExceptions
      if(!d.exists() && !Try(d.mkdirs()).getOrElse(false)) {
        err(s"expected $d to exist")
      } else if(!d.isDirectory) {
        err(s"expected $d to be a directory")
      } else if(!d.canWrite && !Try(d.setWritable(true)).getOrElse(false)) {
        err(s"expected $d to be writeable")
      } else {
        Right(d)
      }
    }

    tempDirParam match {
      //fail if the passed dir isn't valid
      case Some(dir) => checkDir(s"Could not use passed temp dir $tempDirParam", dir)

      //try and generated one otherwise
      case None => {
        Try(Files.createTempDirectory(defaultTempDirPrefix)) match {
          case Success(dir) => checkDir(s"Could not use generated temp dir $dir", dir.toFile)
            .map { x => x.deleteOnExit(); x }
          case Failure(t) => Left(usesTempDirErrorTypeCTOR("Could not create temporary dir").withCause(t))
        }
      }
    }
  }

  def getTempFile(dir: File, suffix: String = defaultSuffix): Either[E, File] = {
    Try(Files.createTempFile(dir.toPath, null, suffix)) match {
      case Success(f) => Right(f.toFile)
      case Failure(t) => Left(
        usesTempDirErrorTypeCTOR(s"Could not create temp file in $dir").withCause(t))
    }
  }

  def stringToTempFile(dir: File, suffix: String = defaultSuffix)(str: String): Either[E, File] = {
    import java.io.PrintWriter
    //close over the XML and write it out to the passed file
    def write(dest: File): Either[E, Unit] = {
      val res = Try(new PrintWriter(dest))
        .map { p =>
          p.println(str)
          p.flush()
          p.close()
        }
      res match {
        case Success(_) => Right(())
        case Failure(t) => Left(
          usesTempDirErrorTypeCTOR(s"Failed to write `$str` to file $dest").withCause(t))
      }
    }

    for {
      f <- getTempFile(dir, suffix)
      _ <- write(f)
    } yield f
  }
}
