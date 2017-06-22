package org.hablapps.datasets

import shapeless._
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import scala.reflect.{classTag, ClassTag}

// A simplified version of the DSL using MTL style, just to show the problems
// that we found while using this approach.

object MTLApproach{

  // DSL algebra

  trait DataSet[D[_]] {
    def source[A](l: List[A]): D[A]
    def expand[A, B](d: D[A])(f: A => TraversableOnce[B]): D[B]
    def filter[A](d: D[A])(f: A => Boolean): D[A]
  }

  // Sample program

  object GenericProgram{
    def program[P[_]](l: List[String])(implicit D: DataSet[P]) =
      D.expand(
        D.filter(
          D.source(l))(
          _.length > 0))(
        _.split(" "))
  }

  // Auxiliary type class that allows us to summon a list of implicit values
  // whose types are specified by a given HList

  trait Implicitly[L <: HList] extends DepFn0{
    type Out = L
  }

  object Implicitly{

    implicit val HNilImplicitly = new Implicitly[HNil]{
      def apply() = HNil
    }

    implicit def HConsImplicitly[A, L <: HList](
        implicit A: A, tail: Implicitly[L]) = new Implicitly[A::L]{
      def apply() = A :: tail()
    }
  }

  // Context-sensitive interpretation

  trait HLRDD[A]{
    type L <: HList
    def apply(ct: L): SparkContext => RDD[A]

    def toRDD(implicit L: Implicitly[L]): SparkContext => RDD[A] = apply(L())
  }

  object HLRDD{

    // Note that we can't specify a common type for `HLRDD#L`, since `source`,
    // `expand` and `filter` each require different types.
    implicit object HLRDDDataSet extends DataSet[HLRDD]{

      def source[A](l: List[A]) = new HLRDD[A]{
        type L = ClassTag[A] :: HNil

        def apply(ct: L) = sc => {
          implicit val _ = ct.head
          sc.parallelize(l)
        }
      }

      def expand[A, B](d: HLRDD[A])(g: A => TraversableOnce[B]) = new HLRDD[B]{
        type L = ClassTag[B] :: d.L

        def apply(ct: L) = sc => {
          implicit val _ = ct.head
          d(ct.tail)(sc) flatMap g
        }
      }

      def filter[A](d: HLRDD[A])(g: A => Boolean) = new HLRDD[A]{
        type L = d.L

        def apply(ct: L) = sc => d(ct)(sc).filter(g)
      }
    }
  }

  // The same program, but implemented for the specific interpretation

  object AdHocProgram{
    import HLRDD.{HLRDDDataSet => D}

    def program(l: List[String]) =
      D.expand(
        D.filter(
          D.source(l))(
          _.length > 0))(
        _.split(" "))
  }

  // Let's try to obtain our RDDs

  object Translation{

    // Translating the ad-hoc program works

    val r1 : SparkContext => RDD[String] =
      AdHocProgram.program(List("ab cd")).toRDD

    // But this doesn't compile since the interpretation
    // doesn't return the required type information.

    // val r2 : SparkContext => RDD[String] =
    //   GenericProgram.program[HLRDD](List("ab cd")).toRDD
  }


}
