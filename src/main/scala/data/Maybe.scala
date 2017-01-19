package com.c12e.learn
package data


import com.c12e.learn.typeclass.{ Equal, Functor, Semigroup }
import com.c12e.learn.typeclass.Equal.Syntax._


// data Maybe[A] = Empty | Just[A](a: A)

sealed trait Maybe[A] {

  def fold[B](ifEmpty: B)(ifJust: A => B): B =
    this match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }

  def map[B](f: A => B): Maybe[B] =
    //fold(Maybe.empty[B])(f andThen Maybe.just)
    //fold(Maybe.empty[B]) { a => Maybe.just(f(a)) }
    this match {
      case Empty() => Maybe.empty[B]
      case Just(a) => Maybe.just(f(a))
    }

}


final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]


object Maybe {

  def empty[A]: Maybe[A] = Empty()

  def just[A](a: A): Maybe[A] = Just(a)

  def fold[A, B](o: Maybe[B])(ifEmpty: A)(ifJust: B => A): A =
    o match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }

  implicit def semigroup[A : Semigroup]: Semigroup[Maybe[A]] =
    new Semigroup[Maybe[A]]{
      def append(a: Maybe[A], b: Maybe[A]): Maybe[A] =
        a.fold(b) { aa =>
          b.fold(a) { bb =>
            just(Semigroup[A].append(aa, bb))
          }
        }
    }

  implicit def functor: Functor[Maybe] =
    new Functor[Maybe] {
      def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
        ma.map(f)
    }

  implicit def equal[A : Equal]: Equal[Maybe[A]] =
    new Equal[Maybe[A]] {
      def equal(ma: Maybe[A], mb: Maybe[A]) =
        ma.fold(mb.fold(true) { _ => false }) { a =>
          mb.fold(false) { b => a === b }
        }
    }

}
