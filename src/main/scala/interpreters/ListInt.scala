package org.hablapps.datasets

/** 
 * An interpreter that runs a dataset program over lists
 */
object ToList extends Interpreter[List]{

  def apply[A](ds: DataSet[A]): List[A] = ds match {
    case e: Expand[_,_,_] => expandToList(e)
    case f: Filter[_,_] => filterToList(f)
    case m: DMap[_,_,_] => mapToList(m)
    case s: SortBy[_,_,_] => sortByToList(s)
    case r: ReduceByKey[_,_,_] => reduceByKeyToList(r)
    case r: GroupByKey[_,_,_] => groupByKeyToList(r)
    case Source(content) => content
  }

  def expandToList[A,B,D<:DataSet[A]](e: Expand[A,B,D]): List[B] =
    apply(e.f) flatMap (e.g)

  def mapToList[A,B,D<:DataSet[A]](e: DMap[A,B,D]): List[B] =
    apply(e.f) map (e.g)

  def filterToList[A,D<:DataSet[A]](f: Filter[A,D]): List[A] =
    apply(f.f) filter f.g

  def reduceByKeyToList[A,B,D<:DataSet[(A,B)]](r: ReduceByKey[A,B,D]): List[(A,B)] = {
    val l: List[(A,B)] = apply(r.f)
    val r1: Map[A, List[(A, B)]] = l.groupBy(_._1)
    r1.mapValues(_.map(_._2).reduce(r.g)).toList
  }

  def sortByToList[A,B,D<:DataSet[A]](s: SortBy[A,B,D]): List[A] = {
    val l = apply(s.f).sortBy(s.g)(s.ord)
    if (s.asc) l else l.reverse
  }

  def groupByKeyToList[A,B,D<:DataSet[(A,B)]](r: GroupByKey[A,B,D]): List[(A,Seq[B])] = {
    val l: List[(A,B)] = apply(r.f)
    val r1: Map[A, List[(A, B)]] = l.groupBy(_._1)
    r1.mapValues(_.map(_._2)).toList
  }
}

