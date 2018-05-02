package com.jakway.util

import java.io.{ByteArrayOutputStream, InputStream}

import com.jakway.gnucash.error.{ValidateF, ValidationError}

import scala.util.{Failure, Success, Try}

object StreamReader {

  //see https://stackoverflow.com/questions/309424/read-convert-an-inputstream-to-a-string
  def jReadStream(in: InputStream): String = {

    val byteArrayOutputStream = new ByteArrayOutputStream()
    val bufferSize = 1024
    val buffer = new Array[Byte](bufferSize)

    var rsz = 0

    do {
      rsz = in.read(buffer, 0, buffer.length)
      if(rsz >= 0) {
        byteArrayOutputStream.write(buffer, 0, rsz)
      }
    } while(rsz >= 0)

    new String(byteArrayOutputStream.toByteArray, "UTF-8")
  }

  val readStream: ValidateF[InputStream, String] =
    (i: InputStream, errorType: String => ValidationError) => {
      Try(jReadStream(i)) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(errorType(s"Failed to read from InputStream $i").withCause(t))
      }
    }

}
