package com.c12e.learn
package test


import org.scalacheck.{ Arbitrary, Gen }

import com.c12e.learn.data._
import com.c12e.learn.typeclass.{ Applicative, Functor }
import com.c12e.learn.typeclass.Functor.Syntax._


trait ArbitraryInstances {

  implicit def genApplicative: Applicative[Gen] =
    new Applicative[Gen] {
      def pure[A](a: A) = Gen const a
      def map[A, B](ga: Gen[A])(f: A => B) = ga map f
      def ap[A, B](ga: Gen[A])(gf: Gen[A => B]) =
        gf flatMap { f => ga map { a => f(a) } }
    }

  implicit def arbitraryApplicative: Applicative[Arbitrary] =
    new Applicative[Arbitrary] {
      def pure[A](a: A) =
        Arbitrary(Applicative[Gen].pure(a))
      def map[A, B](a: Arbitrary[A])(f: A => B) =
        Arbitrary(Functor[Gen].map(a.arbitrary)(f))
      def ap[A, B](a: Arbitrary[A])(f: Arbitrary[A => B]) =
        Arbitrary(Applicative[Gen].ap(a.arbitrary)(f.arbitrary))
    }

  implicit def maxArbitrary[A : Arbitrary]: Arbitrary[Max[A]] =
    arb[A] map Max.apply

  implicit def maybeArbitrary[A : Arbitrary]: Arbitrary[Maybe[A]] =
    arb[Option[A]] map { _.fold(Maybe.empty[A])(Maybe.just) }

  implicit def disjunctionArbitrary[A : Arbitrary, B : Arbitrary]
      : Arbitrary[A \/ B] =
    arb[Either[A, B]] map { _.fold(\/.left, \/.right) }

  // Note: we are piggy-backing on being able to find an arb implemntation of List, and transforming it to IList
  implicit def ilistArbitrary[A : Arbitrary] : Arbitrary[IList[A]] =
    arb[List[A]] map {
      _.foldRight(IList.nil[A]) { IList.cons }
    }

  // Note: we are piggy-backing on being able to find an arb implemntation of List, and transforming it to IList
  implicit def nelArbitrary[A : Arbitrary] : Arbitrary[Nel[A]] =
    // arb[IList[A]] map {
    //   _.fold(arb[A] map (Nel(_)))(t => arb[A] map ( h => Nel(h, t: _*)))
    // }.run
    arb[(A, List[A])] map {
      x => Nel(x._1, x._2: _*)
    }

  private def arb[A](implicit ev: Arbitrary[A]): Arbitrary[A] = ev

}


object ArbitraryInstances extends ArbitraryInstances
