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

object DataSet extends DataSetSyntax

case class Expand[A,B,D<:DataSet[A]](
  f: D, g: A => TraversableOnce[B]) extends DataSet[B]

object Expand{
  implicit def toUnapply[A,B,D<:DataSet[A]] =
    Unapply[B,DataSet,Expand[A,B,D]]
}

case class Filter[A,D<:DataSet[A]](
  f: D, g: A => Boolean) extends DataSet[A]

object Filter{
  implicit def toUnapply[A,D<:DataSet[A]] =
    Unapply[A,DataSet,Filter[A,D]]
}

case class DMap[A,B,D<:DataSet[A]](
  f: D, g: A => B) extends DataSet[B]

object DMap{
  implicit def toUnapply[A,B,D<:DataSet[A]] =
    Unapply[B,DataSet,DMap[A,B,D]]
}

case class ReduceByKey[A,B,D<:DataSet[(A,B)]](
  f: D, g: (B,B)=>B) extends DataSet[(A,B)]

object ReduceByKey{
  implicit def toUnapply[A,B,D<:DataSet[(A,B)]] =
    Unapply[(A,B),DataSet,ReduceByKey[A,B,D]]
}

case class GroupByKey[A,B,D<:DataSet[(A,B)]](
  f: D) extends DataSet[(A,Seq[B])]

object GroupByKey{
  implicit def toUnapply[A,B,D<:DataSet[(A,B)]] =
    Unapply[(A,Seq[B]),DataSet,GroupByKey[A,B,D]]
}

case class SortBy[A,B,D<:DataSet[A]](
  f: D, g: A => B, asc: Boolean = false)(implicit O: Ordering[B]) extends DataSet[A]{
  val ord: Ordering[B] = O
}

object SortBy{
  implicit def toUnapply[A,B,D<:DataSet[A]] =
    Unapply[A,DataSet,SortBy[A,B,D]]
}

case class Source(content: List[String]) extends DataSet[String]

object Source{
  implicit val toUnapply = Unapply[String,DataSet,Source]
}
