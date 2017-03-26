package org.hablapps.datasets

import shapeless.{HNil, ::, HList}

/** 
 * A sample interpreter over HLists that creates the
 * (heterogeneous) list of programs a given data set program
 * is made of.
 */
object ToHList extends CaseInterpreter[DataSet]{

  trait Case[D <: DataSet[_]] extends super.Case[D]{
    type Out <: HList
  }

  type Aux[D <: DataSet[_], L <: HList] = Case[D]{ type Out = L }

  implicit def fromSource: Aux[Source,Source::HNil] =
    new Case[Source]{
      type Out = Source :: HNil
      def apply(d: Source): Out = d :: HNil
    }

  implicit def fromExpand[A,B,D<:DataSet[A]](implicit
    t: Case[D]): Aux[Expand[A,B,D],Expand[A,B,D]::t.Out] =
    new Case[Expand[A,B,D]]{
      type Out = Expand[A,B,D] :: t.Out
      def apply(d: Expand[A,B,D]): Out = d :: t(d.f)
    }

  implicit def fromDMap[A,B,D<:DataSet[A]](implicit
    t: Case[D]): Aux[DMap[A,B,D],DMap[A,B,D]::t.Out] =
    new Case[DMap[A,B,D]]{
      type Out = DMap[A,B,D] :: t.Out
      def apply(d: DMap[A,B,D]): Out = d :: t(d.f)
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit
    t: Case[D]): Aux[Filter[A,D],Filter[A,D]::t.Out] =
    new Case[Filter[A,D]]{
      type Out = Filter[A,D] :: t.Out
      def apply(d: Filter[A,D]): Out = d :: t(d.f)
    }
}
