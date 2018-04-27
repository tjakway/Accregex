package com.jakway.gnucash.test

import com.jakway.gnucash.error.ValidationError
import com.jakway.util.error.UsesTempDir
import org.scalatest.{FlatSpec, Matchers}

object IntegrationTests {
  case class IntegrationTestError(override val msg: String)
    extends ValidationError(msg)
}
import IntegrationTests._

class IntegrationTests
  extends FlatSpec
    with Matchers
    with UsesTempDir[IntegrationTestError] {

}
