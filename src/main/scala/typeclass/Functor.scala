package com.c12e.learn
package typeclass


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}


object Functor {

  @inline def apply[F[_]](implicit ev: Functor[F]): Functor[F] = ev

  class Ops[F[_], A](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit ev: Functor[F]): F[B] =
      ev.map(fa)(f)
  }

  trait Syntax {
    implicit def toFunctorOps[F[_] : Functor, A](fa: F[A]) =
      new Ops(fa)
  }

  object Syntax extends Syntax

  trait Laws {

    import Syntax._
    import Equal.Syntax._

    def functorIdentity[F[_] : Functor, A]
        (fa: F[A])
        (implicit ev: Equal[F[A]]) =
      fa.map(identity) === fa

    def functorComposition[F[_] : Functor, A, B, C]
        (fa: F[A], f: A => B, g: B => C)
        (implicit ev: Equal[F[C]]) =
      fa.map(f).map(g) === fa.map(g compose f)

  }

  object Laws extends Laws

}
