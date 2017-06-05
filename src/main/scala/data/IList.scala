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

  def foldLeft[B](initAcc: B)(f: (B, A) => B): B = this match {
    case Nil() => initAcc
    case Cons(h, t) => f(t.foldLeft(initAcc)(f), h)
  }


  def map[B](f: A => B): IList[B] = fold(nil[B]) {(head: A, acc: IList[B]) => Cons(f(head), acc)}


  def flatMap[B](f: A => IList[B]): IList[B] = fold(nil[B]) { (h: A, t: IList[B]) => f(h) extend t }


  def reverse: IList[A] = foldLeft(nil[A])(_.append(_))

  def reverse1: IList[A] = this match {
    case Nil() => nil
    case Cons(head, tail) => tail.reverse extend cons(head, nil)
  }


  def extend(other: IList[A]): IList[A] = fold(other)(Cons(_,_))

  def extend1(other: IList[A]): IList[A] = this match {
    case Nil() => other
    case Cons(h, t) => Cons(h,t extend1 other)
  }


  def append(a: A): IList[A] = foldLeft(Cons(a, nil))((t, h) => Cons(h, t))

  def append1(a: A): IList[A] = Cons(a, this.reverse).reverse
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

  println(s"Sum using fold of $testList: ${testList.fold(0) {_ + _ }}")
  assert((testList.fold(0) {_ + _ }).equals(6), "fold fail")

  println(s"Reverse $testList: ${testList.reverse}")
  assert(testList.reverse.equals(cons(3, cons(2, cons(1, nil)))), "Reverse fail")

  val flatMapRes: IList[Int] = testList.flatMap { i => Cons(i+1, Cons(i+2, nil))}
  assert(flatMapRes.equals(Cons(2, Cons(3, Cons(3, Cons(4, Cons(4, Cons(5, nil))))))), "flatMap fail")

  val mapRes: IList[String] = testList.map(_.toString)
  assert(mapRes.equals(Cons("1", Cons("2", Cons("3", nil)))), "map fail")

  println(s"Extend list: $testList + $testList = ${testList extend testList}")
  assert((testList extend testList).equals(Cons(1, Cons(2, Cons(3, Cons(1, Cons(2, Cons(3, nil))))))), "Extend list fail")

  assert((testList append 10).equals(Cons(1, Cons(2, Cons(3, Cons(10, nil))))), "Append to list fail")
  println(s"Append 10 to list $testList: ${testList append 10}")

}
