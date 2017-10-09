package com.c12e.learn
package data


import typeclass.{ Equal, Semigroup }


final case class First[A](a: A) extends AnyVal

object First {

  implicit def equal[A : Equal]: Equal[First[A]] =
    Equal[A] contramap { _.a }

  implicit def semigroup[A]: Semigroup[First[A]] =
    new Semigroup[First[A]] {
      def append(first: First[A], second: First[A]) = first
    }

}
