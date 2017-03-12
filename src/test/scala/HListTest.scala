package org.hablapps.datasets
package test

import org.scalatest._

class HListTest extends FunSpec with Matchers{
  import DataSet.syntax._
  import shapeless._

  describe("ToHList tests"){

    it("should work with implicitly"){
      implicitly[ToHList[Source]]
      val t = implicitly[ToHList[Expand[String,Int,Source]]]

      val h /*: Expand[String,Int,Source] :: Source :: HNil*/ =
        t(Source(List("hola")).expand(_ => List(1)))
    }

    it("should work even better with ToHList.apply"){
      val h: Expand[String,Int,Source] :: Source :: HNil =
        ToHList(Source(List("hola")).expand(_ => List(1)))
    }

    // it("word count should work with ToHList"){
    //   val h: DMap[String,(String,Int),Filter[String,Expand[String,String,Source]]] ::
    //          Filter[String,Expand[String,String,Source]] ::
    //          Expand[String,String,Source] ::
    //          Source ::
    //          HNil =
    //     ToHList(wordCount(Source(List("EN UN LUGAR"))))
    // }
  }
}