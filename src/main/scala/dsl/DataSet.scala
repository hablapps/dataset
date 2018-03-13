package org.hablapps.datasets

/**
 * GADT that represents data set transformation programs.
 */
abstract class DataSet[A]{
  type Of[D] = Unapply[D]{
    type P1[t]=DataSet[t]
    type A1 = A
  }
}

case class Expand[A,B,D<:DataSet[A]](
  f: D, g: A => TraversableOnce[B]) extends DataSet[B]

case class Filter[A,D<:DataSet[A]](
  f: D, g: A => Boolean) extends DataSet[A]

case class DMap[A,B,D<:DataSet[A]](
  f: D, g: A => B) extends DataSet[B]

case class MapValues[A,B,C,D<:DataSet[(A,B)]](
  f: D, g: B => C) extends DataSet[(A,C)]

case class ReduceByKey[A,B,D<:DataSet[(A,B)]](
  f: D, g: (B,B)=>B) extends DataSet[(A,B)]

case class GroupByKey[A,B,D<:DataSet[(A,B)]](
  f: D) extends DataSet[(A,Seq[B])]

case class SortBy[A,B,D<:DataSet[A]](
  f: D, g: A => B, asc: Boolean = false)(implicit O: Ordering[B]) extends DataSet[A]{
  val ord: Ordering[B] = O
}

case class Source(content: List[String]) extends DataSet[String]

object DataSet extends DataSetSyntax{
  implicit val toUnapplySource = 
    Unapply[String,DataSet,Source]

  implicit def toUnapplySortBy[A,B,D<:DataSet[A]] =
    Unapply[A,DataSet,SortBy[A,B,D]]

  implicit def toUnapplyGroupByKey[A,B,D<:DataSet[(A,B)]] =
    Unapply[(A,Seq[B]),DataSet,GroupByKey[A,B,D]]

  implicit def toUnapplyReduceyKey[A,B,D<:DataSet[(A,B)]] =
    Unapply[(A,B),DataSet,ReduceByKey[A,B,D]]

  implicit def toUnapplyMapValues[A,B,C,D<:DataSet[(A,B)]] =
    Unapply[(A,C),DataSet,MapValues[A,B,C,D]]

  implicit def toUnapplyFilter[A,D<:DataSet[A]] =
    Unapply[A,DataSet,Filter[A,D]]

  implicit def toUnapplyExpand[A,B,D<:DataSet[A]] =
    Unapply[B,DataSet,Expand[A,B,D]]

  implicit def toUnapplyDMap[A,B,D<:DataSet[A]] =
    Unapply[B,DataSet,DMap[A,B,D]]
}