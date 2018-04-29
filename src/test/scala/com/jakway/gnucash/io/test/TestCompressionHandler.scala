package com.jakway.gnucash.io.test

import java.io.File

import com.jakway.gnucash.Config
import com.jakway.gnucash.io.CompressionHandler
import com.jakway.gnucash.test.{ResourceFiles, TestConfig}
import org.scalatest.{FlatSpec, Matchers}

class TestCompressionHandler
  extends FlatSpec
    with Matchers
    with ResourceFiles
    with TestConfig {

  ignore should "correctly detect compression" in {
    val foodTestFile: File = copyResourceToFile(foodTest)

    CompressionHandler.inputIsCompressed(foodTestFile, quiet) shouldEqual Right(true)
  }
}
