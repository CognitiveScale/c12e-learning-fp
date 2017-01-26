package com.c12e.learn
package data

sealed trait IList[A] {
  //import IList._

  def fold[B](ifNil: B)(ifCons: (A, B) => B): B = ???

}

final case class Nil[A]() extends IList[A]
final case class Cons[A](head: A, tail: IList[A]) extends IList[A]

object IList {

  def nil[A]: IList[A] = Nil[A]
  def cons[A](head: A, tail: IList[A]): IList[A] = Cons[A](head, tail)
}
