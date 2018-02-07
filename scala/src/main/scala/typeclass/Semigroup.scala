package com.c12e.learn
package typeclass


trait Semigroup[A] {
   def append(a1: A, a2: A): A
}


object Semigroup {

  // def implicitly[A](implicit ev: A): A = ev

  @inline def apply[A](implicit ev: Semigroup[A]): Semigroup[A] = ev

  implicit def string: Semigroup[String] =
    new Semigroup[String] {
      def append(x: String, y: String) = x + y
    }

  final class Ops[A](val a: A) extends AnyVal {
    def |+|(a2: A)(implicit ev: Semigroup[A]): A = ev.append(a, a2)
  }

  trait Syntax {
    @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
    implicit def semigroupToOps[A : Semigroup](a: A): Ops[A] =
      new Ops(a)
  }

  object Syntax extends Syntax

  trait Laws {

    import Syntax._
    import Equal.Syntax._

    def semigroupAssociativity[A : Semigroup : Equal]
        (x: A, y: A, z: A): Boolean =
      // implicitly[Semigroup[A]].append(x, implicitly[Semigroup[A]].append(y, z))
      // Semigroup.apply[A].append(x, Semigroup.apply[A].append(y, z))
      // Semigroup[A].append(x, Semigroup[A].append(y, z))
      ((x |+| y) |+| z) === (x |+| (y |+| z))

    def alwaysTrue(i: String, j: String, k: String): Boolean =
      semigroupAssociativity(i, j, k)

  }

  object Laws extends Laws

}
