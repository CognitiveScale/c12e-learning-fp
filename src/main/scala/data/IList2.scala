package com.c12e.learn
package data
/* This is OO Naive encoding of List */

class IList2[A](val isNil: Boolean, head: A, tail: IList2[A]) {

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def foldRight[B](isNil: B, isCons: (A, B) => B): B = {
    if (this.isNil) {
      isNil
    } else {
      isCons(this.head, this.tail.foldRight(isNil, isCons))
    }
  }
}

object IList2 {
  // TODO: Implement this without using Null
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def nil[A](): IList2[A] = new IList2(true, null, null)

  def cons[A](head: A, tail: IList2[A]): IList2[A] = new IList2(false, head, tail)
}


object TestIList2 extends App {
  import IList2._
  var lst:IList[Int] = cons(3, cons(2, cons(2, nil())))
  println(nil())
  println(cons(3, nil()))
  println(cons(3, cons(2, cons(2, nil()))))
}
