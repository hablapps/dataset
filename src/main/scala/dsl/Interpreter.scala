package org.hablapps.datasets

import scalaz.~>

/**
 * Common universal interpreters of data sets in terms of
 * natural transformations.
 */
trait Interpreter[P[_]] extends (DataSet ~> P)

object Interpreter{
  def apply[P[_]](implicit I: Interpreter[P]) = I

  import scalaz.Forall

  implicit def fromCase[P[_]](implicit
    C: Forall[λ[α=>
      CaseInterpreter.EqualTo[P[α]]#Case[DataSet[α]]]]) =
    new Interpreter[P]{
      def apply[A](d: DataSet[A]): P[A] = C[A](d)
    }
}

