package org.hablapps.datasets

/** 
 * GADT that represents data set transformation programs.
 */
abstract class DataSet[A]{
  type Of[D] = DataSet.Unapply[D]{ type A1 = A }
}

object DataSet extends DataSetSyntax{

  /* Unapply */

  import scalaz.Leibniz._

  trait Unapply[D]{
    type A1
    type D1 <: DataSet[A1]
    val leibniz: D === D1
  }

  object Unapply{
    def apply[A,D<:DataSet[A]] =
      new Unapply[D]{
        type D1 = D
        type A1 = A
        val leibniz = refl[D1]
      }
  }
}

case class Expand[A,B,D<:DataSet[A]](
  f: D, g: A => TraversableOnce[B]) extends DataSet[B]

object Expand{
  implicit def toUnapply[A,B,D<:DataSet[A]] =
    DataSet.Unapply[B,Expand[A,B,D]]
}

case class Filter[A,D<:DataSet[A]](
  f: D, g: A => Boolean) extends DataSet[A]

object Filter{
  implicit def toUnapply[A,D<:DataSet[A]] =
    DataSet.Unapply[A,Filter[A,D]]
}

case class DMap[A,B,D<:DataSet[A]](
  f: D, g: A => B) extends DataSet[B]

object DMap{
  implicit def toUnapply[A,B,D<:DataSet[A]] =
    DataSet.Unapply[B,DMap[A,B,D]]
}

case class ReduceByKey[A,B,D<:DataSet[(A,B)]](
  f: D, g: (B,B)=>B) extends DataSet[(A,B)]

object ReduceByKey{
  implicit def toUnapply[A,B,D<:DataSet[(A,B)]] =
    DataSet.Unapply[(A,B),ReduceByKey[A,B,D]]
}

case class GroupByKey[A,B,D<:DataSet[(A,B)]](
  f: D) extends DataSet[(A,Seq[B])]

object GroupByKey{
  implicit def toUnapply[A,B,D<:DataSet[(A,B)]] =
    DataSet.Unapply[(A,Seq[B]),GroupByKey[A,B,D]]
}

case class SortBy[A,B,D<:DataSet[A]](
  f: D, g: A => B, asc: Boolean = false)(implicit O: Ordering[B]) extends DataSet[A]{
  val ord: Ordering[B] = O
}

object SortBy{
  implicit def toUnapply[A,B,D<:DataSet[A]] =
    DataSet.Unapply[A,SortBy[A,B,D]]
}

case class Source(content: List[String]) extends DataSet[String]

object Source{
  implicit val toUnapply = DataSet.Unapply[String,Source]
}
