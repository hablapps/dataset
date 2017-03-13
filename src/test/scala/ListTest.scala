package org.hablapps.datasets
package test

import org.scalatest._

class ListTest extends FunSpec with Matchers{

  describe("List examples"){

    val text: List[String] = List(
      "En un lugar de la mancha",
      "de cuyo nombre no quiero acordarme")

    it("apply"){
      ToList(WordCount(Source(text))) shouldBe WordCount(text)
      // List(
      //   (2,List("de")),
      //   (1,List("En", "acordarme", "cuyo", "la", "lugar", "mancha",
      //           "no", "nombre", "quiero", "un")))
    }
  }

}