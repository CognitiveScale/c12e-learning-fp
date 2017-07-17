package com.c12e.learn
package data


import scala.math.max

import com.c12e.learn.typeclass.{Equal, Semigroup}


final case class Max[A](value: A) extends AnyVal

object Max {

  implicit def equal[A : Equal]: Equal[Max[A]] =
    Equal[A] contramap { _.value }

  implicit def semigroupMaxInt: Semigroup[Max[Int]] = {
      new Semigroup[Max[Int]] {
        def append(x: Max[Int], y: Max[Int]) = Max(max(x.value, y.value))
      }
  }
}
