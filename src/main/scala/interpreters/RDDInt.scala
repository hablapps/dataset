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
    Case[String,Source]{ d => 
      _.parallelize(d.content) 
    }

  implicit def fromExpand[A,B: ClassTag,D<:DataSet[A]](implicit
    t: Case[A,D]) = 
    Case[B,Expand[A,B,D]]{ d => 
      sc => t(d.f)(sc) flatMap d.g
    }

  implicit def fromDMap[A,B: ClassTag,D<:DataSet[A]](implicit
    t: Case[A,D]) =
    Case[B,DMap[A,B,D]]{ d => 
      sc => t(d.f)(sc) map d.g
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit
    t: Case[A,D]) =
    Case[A,Filter[A,D]]{ d => 
      sc => t(d.f)(sc) filter d.g
    }

  implicit def fromSortBy[A,B,D<:DataSet[A]](implicit
    t: Case[A,D],
    ct: ClassTag[B]) =
    Case[A,SortBy[A,B,D]]{ d => 
      sc => t(d.f)(sc).sortBy(d.g,d.asc)(d.ord,ct)
    }

  implicit def fromReduceByKey[A,B,D<:DataSet[(A,B)]](implicit
    t: Case[(A,B),D],
    kt: ClassTag[A], 
    vt: ClassTag[B], 
    ord: Ordering[A]) =
    Case[(A,B),ReduceByKey[A,B,D]]{ d => 
      sc => t(d.f)(sc) reduceByKey d.g
    }

  implicit def fromGroupByKey[A,B,D<:DataSet[(A,B)]](implicit
    t: Case[(A,B),D],
    kt: ClassTag[A], 
    vt: ClassTag[B], 
    ord: Ordering[A]) =
    Case[(A,Seq[B]),GroupByKey[A,B,D]]{ d => 
      sc => t(d.f)(sc).groupByKey().map{ case (k,v) => (k,v.toSeq) }
    }
}