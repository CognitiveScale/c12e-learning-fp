package com.c12e.learn
package typeclass


trait Monoid[A] extends Semigroup[A] {
  def empty : A
}


object Monoid {

  @inline def apply[A](implicit ev: Monoid[A]) = ev

  trait Syntax extends Semigroup.Syntax {
    def empty[A](implicit ev: Monoid[A]): A = ev.empty
  }

  object Syntax extends Syntax

  trait Laws extends Semigroup.Laws {

    import Syntax._
    import Equal.Syntax._

    def monoidLeftIdentity[A : Monoid : Equal](a: A) =
      (empty |+| a) === a

    def monoidRightIdentity[A : Monoid: Equal](a: A) =
      (a |+| empty) === a

  }

  object Laws extends Laws

}
