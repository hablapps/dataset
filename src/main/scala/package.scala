package org.hablapps

package object datasets{

  /* Unapply utils */

  import scalaz.Leibniz._

  trait Unapply[D]{
    type P1[_]
    type A1
    // TODO: use liskov?
    type D1 <: P1[A1]
    val leibniz: D === D1
  }

  object Unapply{
    def apply[A,P[_],D<:P[A]] =
      new Unapply[D]{
        type P1[t] = P[t]
        type A1 = A
        type D1 = D
        val leibniz = refl[D1]
      }
  }

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
    
    object Syntax{
      
      class RunOp[P[_],D<:P[_]](d: D){
        def runWith[I <: CaseInterpreter[P]](i: I)(implicit C: i.Case[D]) = 
          C(d)
      }

      implicit def toRunOp[D](d: D)(implicit U: Unapply[D]) = 
        new RunOp[U.P1,U.D1](U.leibniz(d))
    }
  }

  /* Case-by-case interpreter for given language and constant interpretation */

  trait CaseInterpreterToConstant[P[_]] extends CaseInterpreter[P]{
    type Interpretation

    trait Case[D<:P[_]] extends super.Case[D]{
      type Out <: Interpretation
    }

    object Case{
      def apply[D<:P[_], I<: Interpretation](f: D => I) = 
        new Case[D]{
          type Out = I
          def apply(d: D) = f(d)
        }
    }
  }

  /* Case-by-case interpreter for given language and interpretation */

  trait CaseInterpreterTo[P[_]] extends CaseInterpreter[P]{
    type Interpretation[_]

    trait Case[A,D<:P[A]] extends super.Case[D]{
      type Out = Interpretation[A]
    }

    object Case{
      def apply[A,D<:P[A]](f: D => Interpretation[A]) = 
        new Case[A,D]{
          def apply(d: D) = f(d)
        }
    }
  }

  object CaseInterpreterTo{
    type Aux[P[_],Q[_]] = CaseInterpreterTo[P]{
      type Interpretation[T]=Q[T]
    }

    import scalaz.~>
    implicit def fromUniversal[P[_],Q[_]](implicit nat: P~>Q) = 
      new CaseInterpreterTo[P]{
        type Interpretation[X] = Q[X]
        implicit def univCase[X] = new Case[X,P[X]]{
          def apply(d: P[X]): Interpretation[X] = nat(d)
        }
      }
  }

}