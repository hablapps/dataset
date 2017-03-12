package org.hablapps.datasets
package test

/** 
 * Sample word count transformation
 */
object WordCount{
  import DataSet.syntax._

  def apply[D: DataSet[String]#Of](d: D) =
    d.expand(_.split(" "))
      .filter(! _.isEmpty)
      .map((_,1))
      .reduceByKey[String,Int](_ + _)
      .map(_.swap)
      .groupByKey[Int,String]
      .sortByKey[Int,Seq[String]]()
      .map{ case (k, l) => (k, l.sorted)}

}