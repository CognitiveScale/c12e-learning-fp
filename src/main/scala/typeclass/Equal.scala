package com.c12e.learn
package typeclass


trait Equal[A] { self =>

  def equal(a1: A, a2: A): Boolean

  def contramap[B](f: B => A): Equal[B] =
    new Equal[B] {
      def equal(b1: B, b2: B): Boolean = self.equal(f(b1), f(b2))
    }
}


object Equal {

  @inline def apply[A](implicit ev: Equal[A]): Equal[A] = ev

  def fromObject[A]: Equal[A] =
    new Equal[A] {
      @SuppressWarnings(Array("org.wartremover.warts.Equals"))
      def equal(a1: A, a2: A): Boolean = a1 == a2
    }

  implicit val boolean: Equal[Boolean] = fromObject[Boolean]
  implicit val int: Equal[Int] = fromObject[Int]

  class Ops[A](val a: A) extends AnyVal {

    def ===(b: A)(implicit ev: Equal[A]): Boolean = ev.equal(a, b)

    def =/=(b: A)(implicit ev: Equal[A]): Boolean = ! ev.equal(a, b)

  }

  trait Syntax {
    @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
    implicit def toEqualOps[A : Equal](a: A): Ops[A] = new Ops(a)
  }

  object Syntax extends Syntax

  trait Laws {

    import Syntax._
    import syntax.stdlib.boolean._

    def equalReflexivity[A : Equal](a: A): Boolean =
      // implicitly[Equal[A]].equal(a, a)
      // Equal.apply[A].equal(a, a)
      // Equal[A].equal(a, a)
      a === a

    def equalSymmetry[A : Equal](a: A, b: A): Boolean =
      (a === b) implies (b === a)

    def equalTransitivity[A : Equal](a: A, b: A, c: A): Boolean =
      ((a === b) && (b === c)) implies (a === c)

  }

  object Laws extends Laws

}
