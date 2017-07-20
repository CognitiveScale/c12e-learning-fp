package com.c12e.learn
package data


import com.c12e.learn.typeclass.Equal


final case class Max[A](a: A) extends AnyVal

object Max {

  implicit def equal[A : Equal]: Equal[Max[A]] =
    Equal[A] contramap { _.a }

}
