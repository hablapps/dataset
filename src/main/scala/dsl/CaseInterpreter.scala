package org.hablapps.datasets

import shapeless.DepFn1

/** 
 * Interpreters of data sets in terms of polymorphic functions.
 */
trait CaseInterpreter{

  trait Case[D<:DataSet[_]] extends DepFn1[D]

  def apply[D <: DataSet[_]](d: D)(implicit t: Case[D]): t.Out =
    t(d)
}

object CaseInterpreter{

  object Syntax{
    implicit class RunOp[D<:DataSet[_]](d: D){
      def runWith[I <: CaseInterpreter](i: I)(implicit C: i.Case[D]) = 
        C(d)
    }
  }

  implicit def fromUniversal[P[_]](implicit nat: Interpreter[P]) = 
    new CaseInterpreter{
      implicit def univCase[X] = new Case[DataSet[X]]{
        type Out = P[X]
        def apply(d: DataSet[X]): P[X] = nat(d)
      }
    }

  /* Existential interpreters for specific result types */

  trait EqualTo[P] extends CaseInterpreter{
    trait Case[D<:DataSet[_]] extends DepFn1[D]{
      type Out=P
    }
  }

}
