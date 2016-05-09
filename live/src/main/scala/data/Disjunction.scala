package com.c12e.learn
package data


import com.c12e.learn.typeclass.{ Equal, Functor, Applicative }
import com.c12e.learn.typeclass.Equal.Syntax._


sealed trait \/[A, B] {

  def fold[C](ifLeft: A => C)(ifRight: B => C): C =
    this match {
      case -\/(a) => ifLeft(a)
      case \/-(b) => ifRight(b)
    }

  def map[C](f: B => C): A \/ C =
    fold[A \/ C](-\/(_))(f andThen \/-.apply)

}

final case class -\/[A, B](a: A) extends (A \/ B)
final case class \/-[A, B](b: B) extends (A \/ B)


object \/ {

  def left[A, B](a: A): A \/ B = -\/(a)
  def right[A, B](b: B): A \/ B = \/-(b)

  final class Ops[A](val a: A) extends AnyVal {
    def right[B]: B \/ A = \/-(a)
    def left[B]: A \/ B = -\/(a)
  }

  trait Syntax {
    implicit def any2DisjOps[A](a: A): Ops[A] =
      new Ops(a)
  }

  object Syntax extends Syntax

  implicit def functor[A]: Functor[A \/ ?] =
    new Functor[A \/ ?] {
      def map[B, C](fb: A \/ B)(f: B => C): A \/ C =
        fb map f
    }

  implicit def applicative[A]: Applicative[A \/ ?] =
    new Applicative[A \/ ?] {
      def pure[B](b: B): A \/ B = right(b)

      def ap[B, C](fa: A \/ B)(fab: \/[A, B => C]): A \/ C =
        // If we have a B => C, map it to the fa
        fab.fold(a => left(a): A \/ C)(b_to_c => fa.map(b_to_c))

        // Do we have to implement map twice? Cant we somehow tell scala that we have a functor with map already imeplmented?
      def map[B, C](fb: A \/ B)(f: B => C): A \/ C =
        // Simply pointing the applicatives map to the functors implementaiton
        // Note: We will get an error if map isnt implemented for functor
        implicitly[Functor[A \/ ?]].map(fb)(f)
    }

  implicit def equal[A : Equal, B : Equal]: Equal[A \/ B] =
    new Equal[A \/ B] {
      def equal(d1: A \/ B, d2: A \/ B) =
        d1.fold { a1 =>
          d2.fold { _ === a1 } { _ => false }
        } { b1 =>
          d2.fold { _ => false } { _ === b1 }
        }
    }

}
