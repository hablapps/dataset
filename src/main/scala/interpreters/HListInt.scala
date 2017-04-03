package org.hablapps.datasets

import shapeless.{HNil, ::, HList}

/**
 * A (partial) interpreter over HLists that creates the
 * (heterogeneous) list of programs a given data set program
 * is made of.
 */
object ToHList extends CaseInterpreterConstant[DataSet,HList]{

  implicit def fromSource =
    Case[String,Source]{
      _ :: HNil
    }

  implicit def fromExpand[A,B,D<:DataSet[A]](implicit t: Case[A,D]) =
    Case[B,Expand[A,B,D]]{ d =>
      d :: t(d.f)
    }

  implicit def fromDMap[A,B,D<:DataSet[A]](implicit t: Case[A,D]) =
    Case[B,DMap[A,B,D]]{ d =>
      d :: t(d.f)
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit t: Case[A,D]) =
    Case[A,Filter[A,D]]{ d =>
      d :: t(d.f)
    }
}
