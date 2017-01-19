package com.c12e.learn
package stdlib
package instances


import com.c12e.learn.typeclass.{ Equal, Monoid }


trait StringInstances {

  implicit val stringMonoid: Monoid[String] =
    new Monoid[String] {
      def append(a1: String, a2: String) = a1 + a2
      def empty = ""
    }

  implicit def stringEqual: Equal[String] = Equal.fromObject[String]

}


object StringInstances extends StringInstances
