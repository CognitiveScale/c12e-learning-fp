package com.c12e.learn
package typeclass


trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]

}


object Applicative {

  @inline def apply[F[_]](implicit ev: Applicative[F]): Applicative[F] = ev

  class Ops1[A](val a: A) extends AnyVal {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = ev pure a
  }

  class Ops2[F[_], A](val fa: F[A]) extends AnyVal {
    def <*>[B](f: F[A => B])(implicit ev: Applicative[F]): F[B] = ev.ap(fa)(f)
  }

  trait Syntax extends Functor.Syntax {

    implicit def toApplicativeOps1[A](a: A): Ops1[A] =
      new Ops1(a)

    implicit def toApplicativeOps2[F[_] : Applicative, A]
        (fa: F[A]): Ops2[F, A] =
      new Ops2(fa)

  }

  object Syntax extends Syntax

  object Laws extends Functor.Laws {

    import Syntax._
    import Equal.Syntax._

    def applicativeIdentity[F[_] : Applicative, A]
        (fa: F[A])
        (implicit ev: Equal[F[A]]) =
      (fa <*> (identity[A] _).pure[F]) === fa

    def applicativeHomomorphism[F[_] : Applicative, A, B]
        (a: A, ab: A => B)
        (implicit ev: Equal[F[B]]) =
      (a.pure[F] <*> ab.pure[F]) === ab(a).pure[F]

    def applicativeInterchange[F[_] : Applicative, A, B]
        (a: A, f: F[A => B])
        (implicit ev: Equal[F[B]]) =
      (a.pure[F] <*> f) === (f <*> { (ff: A => B) => ff(a) }.pure[F])

    def applicativeComposition[F[_] : Applicative, A, B, C]
        (fa: F[A], fab: F[A => B], fbc: F[B => C])
        (implicit ev: Equal[F[C]]) = {
      val lhs = (fa <*> fab) <*> fbc
      val rhs =
        (fa <*>
          (fab <*>
            (fbc map { (bc: B => C) => (ab: A => B) => bc compose ab })))
      lhs === rhs
    }

    def applicativeDerivedMap[F[_] : Applicative, A, B]
        (f: A => B, fa: F[A])
        (implicit ev: Equal[F[B]]) =
      fa.map(f) === (fa <*> f.pure[F])

  }

}
