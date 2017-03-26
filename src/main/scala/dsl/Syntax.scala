package org.hablapps.datasets

trait DataSetSyntax{

  object syntax{
    class DataSetOps[A,D<:DataSet[A]](d: D){
      def expand[B](g: A => TraversableOnce[B]) =
        Expand[A,B,D](d,g)

      def filter(pred: A => Boolean) =
        Filter[A,D](d,pred)

      def map[B](f: A => B) =
        DMap[A,B,D](d,f)

      def sortBy[B](f: A => B, asc: Boolean = false)(implicit O: Ordering[B]) =
        SortBy[A,B,D](d, f, asc)

      def reduceByKey[B,C](f: (C,C)=>C)(implicit u: DataSet[(B,C)]#Of[D]) =
        ReduceByKey[B,C,u.D1](u.leibniz(d),f)

      def groupByKey[B,C](implicit u: DataSet[(B,C)]#Of[D]) =
        GroupByKey[B,C,u.D1](u.leibniz(d))

      def sortByKey[B,C](asc: Boolean = false)(implicit u: DataSet[(B,C)]#Of[D], O: Ordering[B]) = {
        DataSetOps(d).sortBy[B](_._1,asc)
      }
    }

    implicit def DataSetOps[D](d: D)(implicit e: Unapply[D]{ type P1[t]=DataSet[t] }) =
      new DataSetOps[e.A1,e.D1](e.leibniz(d))
  }
}