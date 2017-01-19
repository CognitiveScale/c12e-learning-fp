package com.c12e.learn
package typeclass


trait Semigroup[A] {
   def append(a1: A, a2: A): A
}


object Semigroup {

  @inline def apply[A](implicit ev: Semigroup[A]) = ev

  final class Ops[A](val a: A) extends AnyVal {
    def |+|(a2: A)(implicit ev: Semigroup[A]) = ev.append(a, a2)
  }

  trait Syntax {
    implicit def semigroupToOps[A : Semigroup](a: A): Ops[A] =
      new Ops(a)
  }

  object Syntax extends Syntax

  trait Laws {

    import Syntax._
    import Equal.Syntax._

    def semigroupAssociativity[A : Semigroup : Equal](x: A, y: A, z: A) =
      ((x |+| y) |+| z) === (x |+| (y |+| z))

  }

  object Laws extends Laws

}
