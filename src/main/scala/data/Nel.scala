package com.c12e.learn
package data


import com.c12e.learn.typeclass.{ Functor, Semigroup }


sealed abstract class Nel[A] { self =>

  def head: A

  def tail: IList[A]

  def ++(that: Nel[A]): Nel[A] = {
    new Nel[A] {
      def head = self.head
      def tail = self.tail ++ (that.head +: that.tail)
    }
  }

  def map[B](f: A => B) =
    new Nel[B] {
      def head = f(self.head)
      def tail = self.tail map f
    }

}


object Nel {

  def apply[A](h: A, t: A*): Nel[A] =
    new Nel[A] {
      def head = h
      def tail = IList(t: _*)
    }

  implicit def semigroup[A]: Semigroup[Nel[A]] =
    new Semigroup[Nel[A]] {
      def append(a1:Nel[A], a2:Nel[A]) : Nel[A] =
        a1 ++ a2
    }

  implicit val functor: Functor[Nel] =
    new Functor[Nel] {
      def map[A, B](a: Nel[A])(f: A => B): Nel[B] =
        a map f
    }

}
