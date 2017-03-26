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
    Case[Source, Source :: HNil]{ 
      _ :: HNil
    }

  implicit def fromExpand[A,B,D<:DataSet[A]](implicit t: Case[D]) = 
    Case[Expand[A,B,D], Expand[A,B,D] :: t.Out]{ d =>
      d :: t(d.f)
    }

  implicit def fromDMap[A,B,D<:DataSet[A]](implicit t: Case[D]) = 
    Case[DMap[A,B,D], DMap[A,B,D] :: t.Out]{ d => 
      d :: t(d.f)
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit t: Case[D]) = 
    Case[Filter[A,D], Filter[A,D] :: t.Out]{ d => 
      d :: t(d.f)
    }
}
