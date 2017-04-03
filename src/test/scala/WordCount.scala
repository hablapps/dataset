package org.hablapps.datasets
package test

/**
 * Sample word count transformation
 */
object WordCount{
  import DataSet.syntax._

  // Dataset version

  def apply[D: DataSet[String]#Of](d: D) =
    d.expand(_.split(" "))
      .filter(! _.isEmpty)
      .map((_,1))
      .reduceByKey[String,Int](_ + _)
      .map(_.swap)
      .groupByKey[Int,String]
      .sortByKey[Int,Seq[String]]()
      .map{ case (k, l) => (k, l.sorted)}

  // List version

  def apply(d: List[String]) : List[(Int,List[String])] =
    d.flatMap(_.split(" "))
      .filter(! _.isEmpty)
      .map((_,1))
      .groupBy(_._1)
      .mapValues(_.map(_._2).reduce(_+_))
      .toList
      .map(_.swap)
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .toList
      .sortBy(_._1)
      .reverse
      .map{ case (k, l) => (k, l.toList.sorted) }

  // RDD version

  import org.apache.spark.rdd.RDD

  def apply(d: RDD[String]): RDD[(Int,List[String])] =
    d.flatMap(_.split(" "))
      .filter(! _.isEmpty)
      .map((_,1))
      .reduceByKey(_ + _)
      .map(_.swap)
      .groupByKey
      .sortByKey(false)
      .map{ case (k, l) => (k, l.toList.sorted)}

  // // Flink version

  // import org.apache.flink.api.scala.{DataSet => FDataSet, _}
  // import org.apache.flink.api.common.operators.Order

  // def apply(d: FDataSet[String]): FDataSet[(Int, List[String])] =
  //   d.flatMap(_.split(" "))
  //     .filter(!_.isEmpty)
  //     .map((_, 1))
  //     .groupBy(_._1)
  //     .reduce((t1, t2) => (t1._1, t1._2 + t2._2))
  //     .map(_.swap)
  //     .groupBy(_._1)
  //     .reduceGroup(_.toSeq match {
  //       case (k, v) +: rest => (k, v +: rest.map(_._2))
  //     })
  //     .sortPartition(_._1, Order.DESCENDING)
  //     .map{ t => (t._1, t._2.toList.sorted)}

}