package com.c12e.learn
package stdlib
package instances


import com.c12e.learn.data.Max
import com.c12e.learn.typeclass.{ Equal, Monoid }


trait IntInstances {

  implicit val maxIntMonoid: Monoid[Max[Int]] =
    new Monoid[Max[Int]] {
      def append(a1: Max[Int], a2: Max[Int]) = Max(math.max(a1.a, a2.a))
      def empty = Max(Int.MaxValue)
    }

  implicit def intEqual: Equal[Int] = Equal.fromObject[Int]

}


object IntInstances extends IntInstances
