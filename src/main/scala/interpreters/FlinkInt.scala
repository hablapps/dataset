package org.hablapps.datasets

import org.apache.flink.api.scala.{DataSet => FDataSet, _}
import org.apache.flink.api.common.typeinfo.TypeInformation
import scala.reflect.ClassTag

/**
 * A translation of dataset programs into Spark RDDs
 */
object ToFlink extends CaseInterpreterTo[DataSet]{

  type Interpretation[T] = ExecutionEnvironment => FDataSet[T]

  implicit def fromSource(implicit ev: TypeInformation[String]) =
    Case[String, Source] { d =>
      _.fromCollection(d.content)
    }

  implicit def fromExpand[A, B: ClassTag: TypeInformation, D<:DataSet[A]](implicit
      t: Case[A,D]) =
    Case[B, Expand[A, B, D]]{ d =>
      env => t(d.f)(env) flatMap d.g
    }

  implicit def fromDMap[A, B: ClassTag: TypeInformation, D<:DataSet[A]](implicit
      t: Case[A,D]) =
    Case[B, DMap[A, B, D]]{ d =>
      env => t(d.f)(env).map(d.g)
    }

  implicit def fromFilter[A,D<:DataSet[A]](implicit
      t: Case[A,D]) =
    Case[A,Filter[A,D]]{ d =>
      env => t(d.f)(env) filter d.g
    }

  import org.apache.flink.api.common.operators.Order
  implicit def fromSortBy[A, B: TypeInformation, D<:DataSet[A]](implicit
      t: Case[A,D]) =
    Case[A,SortBy[A,B,D]]{ d =>
      // FIXME: Global sorting
      env => t(d.f)(env).sortPartition(d.g, if (d.asc) Order.ASCENDING else Order.DESCENDING)
      // env => t(d.f)(env).partitionByHash(d.g).sortPartition(d.g, if (d.asc) Order.ASCENDING else Order.DESCENDING)
    }

  implicit def fromReduceByKey[A: TypeInformation, B: TypeInformation, D<:DataSet[(A,B)]](implicit
      t: Case[(A,B),D]) =
    Case[(A,B),ReduceByKey[A,B,D]]{ d =>
      env => t(d.f)(env).groupBy(_._1).reduce { (t1, t2) => (t1._1, d.g(t1._2, t2._2)) }
    }

  implicit def fromGroupByKey[A: TypeInformation, B: TypeInformation, D<:DataSet[(A,B)]](implicit
      t: Case[(A,B),D]) =
    Case[(A,Seq[B]),GroupByKey[A,B,D]]{ d =>
      env => t(d.f)(env).groupBy(_._1).reduceGroup(_.toSeq match {
        case (k, v) +: rest => (k, v +: rest.map(_._2))
        case empty => ??? // Unreachable
      })
    }
}
