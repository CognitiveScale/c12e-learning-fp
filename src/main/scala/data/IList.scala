package com.c12e.learn
package data


import scala.annotation.tailrec

import com.c12e.learn.typeclass.{Semigroup, Applicative}


sealed abstract class IList[A] {

  def fold[B](ifNil: B)(ifCons: (A, B) => B): B =
    this match {
      case INil() => ifNil
      case ICons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
    }

  @tailrec
  def foldLeft[B](ifNil: B)(ifCons: (B, A) => B): B =
    this match {
      case INil() => ifNil
      case ICons(h, t) => t.foldLeft(ifCons(ifNil, h))(ifCons)
    }

  def +:(a: A): IList[A] = ICons(a, this)

  def ++(that: IList[A]): IList[A] =
    this match {
      case INil() => that
      case ICons(h, t) => ICons(h, t ++ that)
    }

  def append(that: IList[A]): IList[A] =
    that.fold(this)(IList.cons)

  def map[B](f: A => B): IList[B] =
    fold(IList.nil[B]) { (a, bs) =>
      IList.cons(f(a), bs)
    }

}


final case class INil[A]() extends IList[A]
final case class ICons[A](head: A, tail: IList[A])  extends IList[A]


object IList {

  def apply[A](a: A*): IList[A] =
    a.foldRight(nil[A])(cons)

  def nil[A]: IList[A] = INil()

  def cons[A](head : A, tail: IList[A]): IList[A] = ICons(head, tail)

  // folds should yield identity functions when passed constructors
  def id[A](list: IList[A]): IList[A] =
    list.fold(nil[A])(cons)

  implicit def semigroup[A]: Semigroup[IList[A]] =
    new Semigroup[IList[A]] {
      def append(l1: IList[A], l2: IList[A]) = l1 ++ l2
    }

   implicit def applicative: Applicative[IList] =
     new Applicative[IList] {
       def pure[A](a: A): IList[A] = IList(a)

       def ap[A, B](fa: IList[A])(fab: IList[A => B]): IList[B] =
         fa.fold(IList.nil[B]){
           (head, tail) => fab.map(f => f(head)).append(tail)
         }

       def map[A, B](i: IList[A])(f: A => B): IList[B] = i map f
     }
}
