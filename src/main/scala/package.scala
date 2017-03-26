package org.hablapps

package object datasets{

  /* Universal interpreters */ 

  trait NatTransCompanion[P[_]]{
  
    import scalaz.{~>, Forall}

    implicit def fromCase[Q[_]](implicit
      C: Forall[λ[α=>
        CaseInterpreter[P]#Case[P[α]]{
          type Out = Q[α]
        }]]) =
      new (P~>Q){
        def apply[A](d: P[A]): Q[A] = C[A](d)
      }
  }

  /* Case by case interpreters */
  
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

      import scalaz.~>

      implicit def fromUniversal[Q[_]](implicit nat: P~>Q) = 
        new CaseInterpreter[P]{
          implicit def univCase[X] = new Case[P[X]]{
            type Out = Q[X]
            def apply(d: P[X]): Q[X] = nat(d)
          }
        }
    }
  }


}