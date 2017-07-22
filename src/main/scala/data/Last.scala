package com.c12e.learn
package data

import com.c12e.learn.typeclass.{Equal, Semigroup}


final case class Last[A](value: A) extends AnyVal

object Last {

  implicit def equal[A : Equal]: Equal[Last[A]] =
    Equal[A] contramap { _.value }

  implicit def semigroupLastInt: Semigroup[Last[Int]] = {
    new Semigroup[Last[Int]] {
      def append(x: Last[Int], y: Last[Int]): Last[Int] = y
    }
  }
}