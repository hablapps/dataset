package org.hablapps.datasets

import shapeless.{HNil, ::, HList}

/** 
 * A sample interpreter over HLists that creates the
 * (heterogeneous) list of programs a given data set program
 * is made of.
 */
trait ToHList[D <: DataSet[_]] extends CaseInterpreter[D]{
  type Out <: HList
}

object ToHList extends CaseInterpreter.Companion[ToHList]{

  type Aux[D <: DataSet[_], L <: HList] = ToHList[D]{ type Out = L }

  implicit def fromSource: Aux[Source,Source::HNil] =
    new ToHList[Source]{
      type Out = Source :: HNil
      def apply(d: Source): Out = d :: HNil
    }

  implicit def fromExpand[A,B,D<:DataSet[A]](implicit
    t: ToHList[D]): Aux[Expand[A,B,D],Expand[A,B,D]::t.Out] =
    new ToHList[Expand[A,B,D]]{
      type Out = Expand[A,B,D] :: t.Out
      def apply(d: Expand[A,B,D]): Out = d :: t(d.f)
    }

  implicit def fromDMap[A,B,D<:DataSet[A]](implicit
    t: ToHList[D]): Aux[DMap[A,B,D],DMap[A,B,D]::t.Out] =
    new ToHList[DMap[A,B,D]]{
      type Out = DMap[A,B,D] :: t.Out
      def apply(d: DMap[A,B,D]): Out = d :: t(d.f)
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit
    t: ToHList[D]): Aux[Filter[A,D],Filter[A,D]::t.Out] =
    new ToHList[Filter[A,D]]{
      type Out = Filter[A,D] :: t.Out
      def apply(d: Filter[A,D]): Out = d :: t(d.f)
    }
}
