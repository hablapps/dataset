package org.hablapps.datasets

import org.apache.spark._
import org.apache.spark.rdd.RDD
import scala.reflect.{classTag, ClassTag}

/** 
 * A translation of dataset programs into Spark RDDs
 */
trait ToRDD[A, D<:DataSet[A]] extends CaseInterpreter[D]{
  type Out = ToRDD.Program[A]
}

object ToRDD extends CaseInterpreter.Companion[λ[`D<:DataSet[_]`=>ToRDD[_,D]]]{
  type Program[T] = SparkContext => RDD[T]

  implicit def fromSource = new ToRDD[String,Source]{
    def apply(d: Source): Program[String] =
      _.parallelize(d.content)
  }

  implicit def fromExpand[A,B: ClassTag,D<:DataSet[A]](implicit
    t: ToRDD[A,D]) = new ToRDD[B,Expand[A,B,D]]{
    def apply(d: Expand[A,B,D]): Program[B] =
      sc => t(d.f)(sc) flatMap d.g
  }

  implicit def fromDMap[A,B: ClassTag,D<:DataSet[A]](implicit
    t: ToRDD[A,D]) =
    new ToRDD[B,DMap[A,B,D]]{
      def apply(d: DMap[A,B,D]): Program[B] =
        sc => t(d.f)(sc) map d.g
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit
    t: ToRDD[A,D]) =
    new ToRDD[A,Filter[A,D]]{
      def apply(d: Filter[A,D]): Program[A] =
        sc => t(d.f)(sc) filter d.g
    }

  implicit def fromSortBy[A,B,D<:DataSet[A]](implicit
    t: ToRDD[A,D],
    ct: ClassTag[B]) =
    new ToRDD[A,SortBy[A,B,D]]{
      def apply(d: SortBy[A,B,D]): Program[A] =
        sc => t(d.f)(sc).sortBy(d.g,d.asc)(d.ord,ct)
    }

  implicit def fromReduceByKey[A,B,D<:DataSet[(A,B)]](implicit
    t: ToRDD[(A,B),D],
    kt: ClassTag[A], vt: ClassTag[B], ord: Ordering[A]) =
    new ToRDD[(A,B),ReduceByKey[A,B,D]]{
      def apply(d: ReduceByKey[A,B,D]): Program[(A,B)] =
        sc => t(d.f)(sc) reduceByKey d.g
    }

  implicit def fromGroupByKey[A,B,D<:DataSet[(A,B)]](implicit
    t: ToRDD[(A,B),D],
    kt: ClassTag[A], vt: ClassTag[B], ord: Ordering[A]) =
    new ToRDD[(A,Seq[B]),GroupByKey[A,B,D]]{
      def apply(d: GroupByKey[A,B,D]): Program[(A,Seq[B])] =
        sc => t(d.f)(sc).groupByKey().map{ case (k,v) => (k,v.toSeq) }
    }
}