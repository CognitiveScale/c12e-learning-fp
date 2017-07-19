package com.c12e.learn
package data


import com.c12e.learn.typeclass.Semigroup

final case class First[A](value: A) extends AnyVal

object First {
  
  implicit def semigroupFirst[A]: Semigroup[First[A]] = {
    new Semigroup[First[A]] {
      def append(x: First[A], y: First[A]) = x
    }
  }

  // First(None) |+| First(Some(2)) not working...
  implicit def semigroupFirstOption[A: Semigroup]: Semigroup[Option[A]] = {
    new Semigroup[Option[A]] {
      def append(x: Option[A], y: Option[A]) = {
        (x, y) match {
          case (Some(_), Some(_)) => x
          case (Some(_), None) => x
          case (None, Some(_)) => y
          case (None, None) => None
        }
      }
    }
  }
}
