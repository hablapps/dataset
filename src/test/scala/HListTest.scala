package org.hablapps.datasets
package test

import org.scalatest._

class HListTest extends FunSpec with Matchers{
  import DataSet.syntax._
  import shapeless._

  describe("ToHList tests"){

    it("should work with implicitly"){
      implicitly[ToHList.Case[Source]]
      val t = implicitly[ToHList.Case[Expand[String,Int,Source]]]

      val h /*: Expand[String,Int,Source] :: Source :: HNil*/ =
        t(Source(List("hola")).expand(_ => List(1)))
    }

    it("should work even better with ToHList.apply"){
      val h: Expand[String,Int,Source] :: Source :: HNil =
        ToHList(Source(List("hola")).expand(_ => List(1)))
    }
  }
}