package com.c12e.learn
package test


import org.scalacheck.{ Arbitrary, Properties }
import org.scalacheck.Prop.forAll

import com.c12e.learn.typeclass._


object Props {

  def equal[A : Equal : Arbitrary] =
    new Properties("equal") {
      property("reflexivity") = forAll(Equal.Laws.equalReflexivity[A] _)
      property("symmetry") = forAll(Equal.Laws.equalSymmetry[A] _)
      property("transitivity") = forAll(Equal.Laws.equalTransitivity[A] _)
    }

  def semigroup[A : Semigroup : Equal : Arbitrary] =
    new Properties("semigroup") {
      property("associativity") =
        forAll(Semigroup.Laws.semigroupAssociativity[A] _)
    }

  def monoid[A : Monoid : Equal : Arbitrary] =
    new Properties("monoid") {
      include(semigroup)
      property("left_identity") = forAll(Monoid.Laws.monoidLeftIdentity[A] _)
      property("right_identity") = forAll(Monoid.Laws.monoidRightIdentity[A] _)
    }

  def functor[F[_] : Functor, A, B, C]
      (implicit
        ev1: Equal[F[A]],
        ev2: Equal[F[C]],
        ev3: Arbitrary[F[A]],
        ev4: Arbitrary[A => B],
        ev5: Arbitrary[B => C]) =
    new Properties("functor") {
      property("identity") =
        forAll(Functor.Laws.functorIdentity[F, A] _)
      property("composition") =
        forAll(Functor.Laws.functorComposition[F, A, B, C] _)
    }

  def applicative[F[_] : Applicative, A, B, C]
      (implicit
        ev1: Equal[F[A]],
        ev2: Equal[F[B]],
        ev3: Equal[F[C]],
        ev4: Arbitrary[A],
        ev5: Arbitrary[F[A]],
        ev6: Arbitrary[A => B],
        ev7: Arbitrary[F[A => B]],
        ev8: Arbitrary[B => C],
        ev9: Arbitrary[F[B => C]]) =
    new Properties("applicative") {
      include(functor[F, A, B, C])
      property("identity") =
        forAll(Applicative.Laws.applicativeIdentity[F, A] _)
      property("homomorphism") =
        forAll(Applicative.Laws.applicativeHomomorphism[F, A, B] _)
      property("interchange") =
        forAll(Applicative.Laws.applicativeInterchange[F, A, B] _)
      property("composition") =
        forAll(Applicative.Laws.applicativeComposition[F, A, B, C] _)
      property("derived_map") =
        forAll(Applicative.Laws.applicativeDerivedMap[F, A, B] _)
    }

}
