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
      .mapValues{ (l: Seq[String]) => l.sorted }
      .sortByKey[Int,Seq[String]]()

  // List version

  def apply(d: List[String]): List[(Int,List[String])] =
    d.flatMap(_.split(" "))
      .filter(! _.isEmpty)
      .map((_,1))
      .groupBy(_._1)
      .mapValues(_.map(_._2).reduce(_+_))
      .toList
      .map(_.swap)
      .groupBy(_._1)
      .mapValues(_.map(_._2).toList.sorted)
      .toList
      .sortBy(_._1)(Ordering[Int].reverse)

  // RDD version

  import org.apache.spark.rdd.RDD

  def apply(d: RDD[String]): RDD[(Int,List[String])] =
    d.flatMap(_.split(" "))
      .filter(! _.isEmpty)
      .map((_,1))
      .reduceByKey(_ + _)
      .map(_.swap)
      .groupByKey
      .mapValues{ _.toList.sorted }
      .sortByKey(false)
}