package org.hablapps.datasets

import scalaz.~>

/**
 * Common universal interpreters of data sets in terms of
 * natural transformations.
 */

trait Interpreter[P[_]] extends (DataSet ~> P)

object Interpreter extends NatTransCompanion[DataSet]