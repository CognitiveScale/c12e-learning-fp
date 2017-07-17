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

  def foldLeft1[B](initAcc: B)(ifCons: (B, A) => B): B = 
    this match {
      case Nil() => initAcc
      case Cons(h, t) => t.foldLeft1(ifCons(initAcc, h))(ifCons)
    }

  //def foldLeft2[B](initAcc: B)(ifCons: (B, A) => B): B = {
    //val initAcc = 
    //fold(initAcc)(ifCons)
  //}

  def map[B](f: A => B): IList[B] = fold(nil[B])((a, b) => cons(f(a), b))

  def reverse: IList[A] = foldLeft1(nil[A])((b, a) => cons(a, b))

  def flatMap[B](f: A => IList[B]): IList[B] = IList.join(map(f))

  def reverse: IList[A] = foldLeft(nil[A]) { (t, h) => cons(h, t) }
}

final case class Nil[A]() extends IList[A]
final case class Cons[A](head: A, tail: IList[A]) extends IList[A]


object IList {

  def nil[A]: IList[A] = Nil[A]()
  def cons[A](head: A, tail: IList[A]): IList[A] = Cons[A](head, tail)

  def join[A](lst: IList[IList[A]]): IList[A] = {
    lst match {
      case Nil() => nil
      case Cons(hs, ts) => cat(hs, join(ts))
    }
  }

  def cat[A](l1: IList[A], l2: IList[A]): IList[A] = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => cons(h, cat(t, l2))
    }
  }
}


object IListTest extends App {
  def mkList[A](a: A, b: A, c: A): IList[A] =
    cons(a, cons(b, cons(c, nil)))
  val testList: IList[Int] = cons(1, cons(2, cons(3, nil)))
  println(testList)
  println(testList.fold(0) {_ + _ })
  println(testList.map(2 + _))
  println("foldLeft: ")
  println(testList.foldLeft1(nil[Int])((b, a) => cons(a, b)))
  println(testList.foldLeft1(0)(_ + _))
  print("reverse: ")
  println(testList.reverse)
  print("cat: ")
  println(IList.cat(testList, cons(4, cons(5, nil))))
  print("join: ")
  println(IList.join(cons(testList, cons(testList, nil))))
//  println(testList.foldLeft2(nil[Int])((b, a) => cons(a, b)))
  print("flatMap: ")
  println(testList.flatMap(x => cons(x-1, cons(x+1, nil))))
}
