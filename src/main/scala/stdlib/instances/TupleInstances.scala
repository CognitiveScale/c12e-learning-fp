package com.c12e.learn
package stdlib
package instances


import com.c12e.learn.typeclass.{ Equal, Monoid }


trait TupleInstances {

  implicit def tupleMonoid[A : Monoid, B : Monoid]: Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def append(p1: (A, B), p2: (A, B)) =
        (Monoid[A].append(p1._1, p2._1), Monoid[B].append(p1._2, p2._2))
      def empty = (Monoid[A].empty, Monoid[B].empty)
    }

  implicit def tupleEqual[A : Equal, B : Equal]: Equal[(A, B)] =
    new Equal[(A, B)] {
      def equal(t1: (A, B), t2: (A, B)) =
        Equal[A].equal(t1._1, t2._1) && Equal[B].equal(t1._2, t2._2)
    }

}


object TupleInstances extends TupleInstances
