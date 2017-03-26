package org.hablapps.datasets

import org.apache.spark._
import org.apache.spark.rdd.RDD
import scala.reflect.{classTag, ClassTag}

/** 
 * A translation of dataset programs into Spark RDDs
 */
object ToRDD extends CaseInterpreterTo[DataSet]{

  type Interpretation[T] = SparkContext => RDD[T]

  implicit def fromSource = 
    new Case[String,Source]{
      def apply(d: Source): Interpretation[String] =
        _.parallelize(d.content)
    }

  implicit def fromExpand[A,B: ClassTag,D<:DataSet[A]](implicit
    t: Case[A,D]) = 
    new Case[B,Expand[A,B,D]]{
      def apply(d: Expand[A,B,D]): Interpretation[B] =
        sc => t(d.f)(sc) flatMap d.g
    }

  implicit def fromDMap[A,B: ClassTag,D<:DataSet[A]](implicit
    t: Case[A,D]) =
    new Case[B,DMap[A,B,D]]{
      def apply(d: DMap[A,B,D]): Interpretation[B] =
        sc => t(d.f)(sc) map d.g
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit
    t: Case[A,D]) =
    new Case[A,Filter[A,D]]{
      def apply(d: Filter[A,D]): Interpretation[A] =
        sc => t(d.f)(sc) filter d.g
    }

  implicit def fromSortBy[A,B,D<:DataSet[A]](implicit
    t: Case[A,D],
    ct: ClassTag[B]) =
    new Case[A,SortBy[A,B,D]]{
      def apply(d: SortBy[A,B,D]): Interpretation[A] =
        sc => t(d.f)(sc).sortBy(d.g,d.asc)(d.ord,ct)
    }

  implicit def fromReduceByKey[A,B,D<:DataSet[(A,B)]](implicit
    t: Case[(A,B),D],
    kt: ClassTag[A], 
    vt: ClassTag[B], 
    ord: Ordering[A]) =
    new Case[(A,B),ReduceByKey[A,B,D]]{
      def apply(d: ReduceByKey[A,B,D]): Interpretation[(A,B)] =
        sc => t(d.f)(sc) reduceByKey d.g
    }

  implicit def fromGroupByKey[A,B,D<:DataSet[(A,B)]](implicit
    t: Case[(A,B),D],
    kt: ClassTag[A], 
    vt: ClassTag[B], 
    ord: Ordering[A]) =
    new Case[(A,Seq[B]),GroupByKey[A,B,D]]{
      def apply(d: GroupByKey[A,B,D]): Interpretation[(A,Seq[B])] =
        sc => t(d.f)(sc).groupByKey().map{ case (k,v) => (k,v.toSeq) }
    }
}