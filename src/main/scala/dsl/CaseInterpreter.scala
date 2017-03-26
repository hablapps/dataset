package org.hablapps.datasets

import shapeless.DepFn1

/** 
 * Interpreters of data sets in terms of polymorphic functions.
 */
trait DataSetCaseInterpreter extends CaseInterpreter[DataSet]

object DataSetCaseInterpreter extends CaseInterpreter.Companion[DataSet]
