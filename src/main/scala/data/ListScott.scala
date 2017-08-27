/**
  * Created by akumar on 24/08/17.
  */


package com.c12e.learn
package data


sealed trait ListScott[A] {

  import ListScott.{nil, cons}

  def imatch[B](b: B)(f: (A, ListScott[A]) => B): B

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def fold[B](ifNil: B, ifCons: (A, B) => B): B =
    imatch(ifNil)((h, t) => ifCons(h, t.fold(ifNil, ifCons)))

  def ++(l: ListScott[A]): ListScott[A] = fold(l, cons)

  def map[B](f: A => B): ListScott[B] =
    fold(nil[B](), (h, t: ListScott[B]) => cons(f(h), t))

  def flatMap[B](f: A => ListScott[B]): ListScott[B] =
    fold(nil[B](), (h, t: ListScott[B]) => f(h) ++ t)

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String = fold[String]("", (h, t) => h.toString + " " + t)
}


object ListScott {


  def nil[A](): ListScott[A] =
    new ListScott[A] {
      override def imatch[B](b: B)(f: (A, ListScott[A]) => B): B = b
    }


  def cons[A](head: A, tail: ListScott[A]): ListScott[A] =
    new ListScott[A] {
      override def imatch[B](b: B)(f: (A, ListScott[A]) => B): B = f(head, tail)
    }


}

object ListScottTest extends App {

  import ListScott.{nil, cons}

  val emptyList: ListScott[Int] = nil()
  val list: ListScott[Int] = cons(1, cons(2, cons(3, nil())))

  val sum: Int = list.fold(0, (x: Int, y: Int) => x + y)


  println(list)
  println(list.flatMap(a => cons(a + 1, nil())))
  println(sum)
}
