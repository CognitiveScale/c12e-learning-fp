package com.c12e.learn
package data

sealed trait Maybe[A]{
  import Maybe.{empty, just}

  def fold[B](ifEmpty: B)(ifJust: A => B): B = 
    this match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }

  def map[B](f: A => B): Maybe[B] =
    fold(empty[B])(a => just(f(a)))

  def ap[B](mf: Maybe[A => B]): Maybe[B] = {
    mf.fold(empty[B])(map _)
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = 
    fold(empty[B])(f)
}


final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]

object Maybe {

  def pure[A](a: A): Maybe[A] = Just(a)

  // "smart" construtor
  def empty[A](): Maybe[A] = Empty[A]

  def just[A](a: A): Maybe[A] = Just(a)
}

object MaybeRun extends App{
  val x = Just(23)
  println(x)
  println(x.a)
  println(x.flatMap((x => Just(x*2))))

}
