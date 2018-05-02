package com.jakway.gnucash.test

import com.jakway.gnucash.error.ValidationError

/**
  * A type for errors that arise because of problems in the test suite,
  * not due to failure of production code under test
  * @param msg
  */
class TestError(override val msg: String)
  extends ValidationError(msg)


class AccregexTestSetupException(override val msg: String)
  extends TestError(msg)
