package com.c12e.learn
package data


import IList.{ nil, cons }

// data List a = Nil | Cons a (List a)

sealed trait IList[A] {

  def fold[B](ifNil: B)(ifCons: (A, B) => B): B =
    this match {
      case Nil() => ifNil
      case Cons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
    }

}

final case class Nil[A]() extends IList[A]
final case class Cons[A](head: A, tail: IList[A]) extends IList[A]


object IList {
  def nil[A]: IList[A] = Nil[A]()
  def cons[A](head: A, tail: IList[A]): IList[A] = Cons[A](head, tail)
}


object IListTest extends App {
  val testList: IList[Int] = cons(1, cons(2, cons(3, nil)))
  println(testList)
  println(testList.fold(0) {_ + _ })
}
