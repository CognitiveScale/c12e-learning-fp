package com.c12e.learn
package typeclass


import com.c12e.learn.stdlib.syntax.BooleanSyntax


trait Equal[A] { self =>

  def equal(a1: A, a2: A): Boolean

  def contramap[B](f: B => A): Equal[B] =
    new Equal[B] {
      def equal(b1: B, b2: B) = self.equal(f(b1), f(b2))
    }

}


object Equal {

  @inline def apply[A](implicit ev: Equal[A]): Equal[A] = ev

  def fromObject[A]: Equal[A] =
    new Equal[A] {
      def equal(a1: A, a2: A) = a1 == a2
    }

  class Ops[A](val a: A) extends AnyVal {

    def ===(b: A)(implicit ev: Equal[A]): Boolean = ev.equal(a, b)

    def =/=(b: A)(implicit ev: Equal[A]): Boolean = ! ev.equal(a, b)

  }

  trait Syntax {
    implicit def toEqualOps[A : Equal](a: A): Ops[A] = new Ops(a)
  }

  object Syntax extends Syntax

  trait Laws {

    import Syntax._
    import BooleanSyntax._

    def equalReflexivity[A : Equal](a: A) =
      a === a

    def equalSymmetry[A : Equal](a: A, b: A) =
      (a === b) implies (b === a)

    def equalTransitivity[A : Equal](a: A, b: A, c: A) =
      ((a === b) && (b === c)) implies (a === c)

  }

  object Laws extends Laws

}
