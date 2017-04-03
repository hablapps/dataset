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


  /* Case-by-case interpreter for given language and constant interpretation */
  import shapeless._

  trait CaseInterpreterConstant[P[_],Q] extends Poly{

    type Case[D<:P[_]] = poly.Case[this.type, D::HNil]{
      type Result <: Q
    }

    object Case{
      case class Builder[D<:P[_]](){
        def apply[I<:Q](f: D => I) =
        ProductCase[D::HNil,I]{ l => f(l.head) }
      }
      def apply[D<:P[_]] = Builder[D]()
    }
  }

  /* Case-by-case interpreter for given language and interpretation */

  trait CaseInterpreter[P[_],Q[_]] extends Poly{

    type Case[A,D<:P[A]] = poly.Case[this.type, D::HNil]{
      type Result = Q[A]
    }

    object Case{
      def apply[A,D<:P[A]](f: D => Q[A]) =
        ProductCase[D::HNil,Q[A]]{
          l => f(l.head)
        }
    }
  }


}