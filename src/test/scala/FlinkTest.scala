// package org.hablapps.datasets
// package test

// import org.scalatest.{Filter =>_, _}

// class FlinkTest extends FunSpec with Matchers {

//   // Start flink environment

//   import org.apache.flink.api.java.io.LocalCollectionOutputFormat
//   import org.apache.flink.api.scala.{DataSet => FDataSet, _}

//   val env = ExecutionEnvironment.getExecutionEnvironment

//   // Different tests

//   describe("Test DSs"){

//     it("automatically"){
//       val text: List[String] = List(
//         "En un lugar de la mancha",
//         "de cuyo nombre no quiero acordarme")

//       // Interpretation of common program

//       val wcDS = ToFlink(WordCount(Source(text))).apply(env)
//       val result = new java.util.ArrayList[(Int, Seq[String])]()
//       wcDS.output(new LocalCollectionOutputFormat(result))

//       // Execution of flink implementation

//       val wcDS2 = WordCount(env.fromCollection(text))
//       val result2 = new java.util.ArrayList[(Int, List[String])]()
//       wcDS2.output(new LocalCollectionOutputFormat(result2))

//       // Results should be the same

//       env.execute()

//       import collection.JavaConverters._
//       result.asScala.toList.sortBy(_._1) shouldBe
//         result2.asScala.toList.sortBy(_._1)
//     }
//   }

// }