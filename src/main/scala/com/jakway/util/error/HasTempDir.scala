package com.jakway.util.error

import java.io.File
import java.nio.file.Files

import scala.util.{Failure, Success, Try}

trait UsesTempDir[E <: WithCause[E]] {
  val tempDirParam: Option[File]
  val defaultTempDirPrefix: String

  def usesTempDirErrorTypeCTOR: String => E = (x: String) => new E(x)

  def getTmpDir(): Either[E, File] = {

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

  def getTempFile(dir: File): Either[E, File] = {
    Try(Files.createTempFile(dir.toPath, null, ".xml")) match {
      case Success(f) => Right(f.toFile)
      case Failure(t) => Left(
        usesTempDirErrorTypeCTOR(s"Could not create temp file in $dir").withCause(t))
    }
  }

  def stringToTempFile(dir: File)(str: String): Either[E, File] = {
    import java.io.PrintWriter
    //close over the XML and write it out to the passed file
    def write(dest: File): Either[E, Unit] = {
      val res = Try(new PrintWriter(dest))
        .map { p =>
          p.println(str)
          p.close()
        }
      res match {
        case Success(_) => Right(())
        case Failure(t) => Left(
          usesTempDirErrorTypeCTOR(s"Failed to write `$str` to file $dest").withCause(t))
      }
    }

    for {
      f <- getTempFile(dir)
      _ <- write(f)
    } yield f
  }
}
