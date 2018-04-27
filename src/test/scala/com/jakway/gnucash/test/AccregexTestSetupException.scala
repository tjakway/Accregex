package com.jakway.gnucash.test

import com.jakway.gnucash.error.ValidationError
import com.jakway.util.error.WithCause

class AccregexTestSetupException(override val msg: String)
  extends ValidationError(msg)
