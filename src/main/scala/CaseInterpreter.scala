package org.hablapps.datasets

import shapeless.DepFn1

/** 
 * Interpreters of data sets in terms of dependent functions.
 */
trait CaseInterpreter[D<:DataSet[_]] extends DepFn1[D]

object CaseInterpreter{

  trait Companion[I[D<:DataSet[_]]<:CaseInterpreter[D]]{
    def apply[D <: DataSet[_]](d: D)(implicit t: I[D]): t.Out =
      t(d)
  }

  /* Existential interpreters for specific result types */

  trait EqualTo[D<:DataSet[_],P] extends CaseInterpreter[D]{
    type Out=P
  }
}
