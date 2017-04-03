package org.hablapps.datasets
package test

import org.scalatest._

class HListTest extends FunSpec with Matchers{
  import DataSet.syntax._
  import shapeless._

  describe("ToHList tests"){

    it("should work with implicitly"){
      implicitly[ToHList.Case[String,Source]]
      val t = implicitly[ToHList.Case[Int,Expand[String,Int,Source]]]

      val h /*: Expand[String,Int,Source] :: Source :: HNil*/ =
        t(Source(List("hola")).expand(_ => List(1)))
    }

    it("should work even better with ToHList.apply"){
      // Gets confused with Poly#apply[R](implicit c : ProductCase.Aux[HNil, R]) : R
      // if we are explicit about the type of `h`
      val h = ToHList(Source(List("hola")).expand(_ => List(1)))

      val h1: Expand[String,Int,Source] :: Source :: HNil = h
    }
  }
}