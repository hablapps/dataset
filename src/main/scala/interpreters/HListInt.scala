package org.hablapps.datasets

import shapeless.{HNil, ::, HList}

/** 
 * A (partial) interpreter over HLists that creates the
 * (heterogeneous) list of programs a given data set program
 * is made of.
 */
object ToHList extends CaseInterpreterToConstant[DataSet]{

  type Interpretation = HList

  implicit def fromSource = 
    new Case[Source]{
      type Out = Source :: HNil
      def apply(d: Source): Out = d :: HNil
    }

  implicit def fromExpand[A,B,D<:DataSet[A]](implicit t: Case[D]) = 
    new Case[Expand[A,B,D]]{
      type Out = Expand[A,B,D] :: t.Out
      def apply(d: Expand[A,B,D]): Out = d :: t(d.f)
    }

  implicit def fromDMap[A,B,D<:DataSet[A]](implicit t: Case[D]) = 
    new Case[DMap[A,B,D]]{
      type Out = DMap[A,B,D] :: t.Out
      def apply(d: DMap[A,B,D]): Out = d :: t(d.f)
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit t: Case[D]) = 
    new Case[Filter[A,D]]{
      type Out = Filter[A,D] :: t.Out
      def apply(d: Filter[A,D]): Out = d :: t(d.f)
    }
}
