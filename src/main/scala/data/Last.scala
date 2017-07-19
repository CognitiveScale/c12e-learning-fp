package com.c12e.learn
package data

import com.c12e.learn.typeclass.Semigroup

final case class Last[A](value: A) extends AnyVal

object Last {

  implicit def semigroupLast[A]: Semigroup[Last[A]] = {
    new Semigroup[Last[A]] {
      def append(x: Last[A], y: Last[A]) = y
    }
  }
}

