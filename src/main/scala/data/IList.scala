package com.c12e.learn
package data


import com.c12e.learn.typeclass.Semigroup

import IList.{ nil, cons }


// data List a = Nil | Cons a (List a)

sealed trait IList[A] {

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def fold[B](ifNil: B)(ifCons: (A, B) => B): B =
    this match {
      case Nil() => ifNil
      case Cons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
    }

  def ++(l: IList[A]): IList[A] = fold(l)(cons)

  def map[B](f: A => B): IList[B] =
    // this match {
    //   case Nil() => nil[B]
    //   case Cons(h, t) => Cons(f(h), t.map(f))
    // }
    fold(nil[B]) { (a, b) => cons(f(a), b) }

  def flatMap[B](f: A => IList[B]): IList[B] =
    fold(nil[B]) { (a, b) => f(a) ++ b }

  def flatten[B](implicit ev: A =:= IList[B]): IList[B] =
    flatMap(ev)

  def foldLeft[B](initAcc: B)(f: (B, A) => B): B =
    fold[B => B](identity){ (a, acc) => b => acc(f(b, a)) }(initAcc)

  def reverse: IList[A] = foldLeft(nil[A]) { (t, h) => cons(h, t) }
}

final case class Nil[A]() extends IList[A]
final case class Cons[A](head: A, tail: IList[A]) extends IList[A]


object IList {

  def nil[A]: IList[A] = Nil[A]()
  def cons[A](head: A, tail: IList[A]): IList[A] = Cons[A](head, tail)

  implicit def semigroup[A]: Semigroup[IList[A]] =
    new Semigroup[IList[A]] {
      def append(x: IList[A], y: IList[A]) = x ++ y
    }

}


object IListTest extends App {
  def mkList[A](a: A, b: A, c: A): IList[A] =
    cons(a, cons(b, cons(c, nil)))
  val testList: IList[Int] = cons(1, cons(2, cons(3, nil)))
  println(testList)
  println(testList.fold(0) {_ + _ })
  println(testList.reverse)
  println(mkList(mkList(1,2,3), mkList(4,5,6), mkList(7,8,9)).flatten[Int])
}
