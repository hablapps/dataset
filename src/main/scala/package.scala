package org.hablapps

package object datasets{

  /* Universal interpreters */ 

  trait NatTransCompanion[P[_]]{
  
    import scalaz.{~>, Forall}

    implicit def fromCase[Q[_]](implicit
      C: Forall[λ[α=> CaseInterpreterTo.Aux[P,Q]#Case[α,P[α]]]]) = 
      λ[P~>Q]{ C.apply(_) }
  }

  /* Case by case interpreters for language P[_] */
  
  trait CaseInterpreter[P[_]]{

    trait Case[D<:P[_]] extends shapeless.DepFn1[D]

    def apply[D<:P[_]](d: D)(implicit t: Case[D]): t.Out =
      t(d)
  }

  object CaseInterpreter{

    trait Companion[P[_]]{

      object Syntax{
        implicit class RunOp[D<:P[_]](d: D){
          def runWith[I <: CaseInterpreter[P]](i: I)(implicit C: i.Case[D]) = 
            C(d)
        }
      }

    }
  }

  /* Case-by-case interpreter for given language and constant interpretation */

  trait CaseInterpreterToConstant[P[_]] extends CaseInterpreter[P]{
    type Interpretation

    trait Case[D<:P[_]] extends super.Case[D]{
      type Out <: Interpretation
    }
  }

  /* Case-by-case interpreter for given language and interpretation */

  trait CaseInterpreterTo[P[_]] extends CaseInterpreter[P]{
    type Interpretation[_]

    trait Case[A,D<:P[A]] extends super.Case[D]{
      type Out = Interpretation[A]
    }
  }

  object CaseInterpreterTo{
    type Aux[P[_],Q[_]] = CaseInterpreterTo[P]{
      type Interpretation[T]=Q[T]
    }

    trait Companion[P[_]] extends CaseInterpreter.Companion[P]{
      import scalaz.~>

      implicit def fromUniversal[Q[_]](implicit nat: P~>Q) = 
        new CaseInterpreterTo[P]{
          type Interpretation[X] = Q[X]
          implicit def univCase[X] = new Case[X,P[X]]{
            def apply(d: P[X]): Interpretation[X] = nat(d)
          }
        }
    }
  }

}