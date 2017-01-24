package com.c12e.learn
package data


sealed trait Maybe[A] {

  import Maybe.{empty, just}

  def fold[B](ifEmpty: B)(ifJust: A => B): B =
    this match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }

  def map[B](f: A => B): Maybe[B] =
    //fold[Maybe[B]](Maybe.empty[B])(a => Maybe.just(f(a)))
    //fold(empty[B]) { a => just(f(a)) }
    fold(empty[B])(f andThen just)

  def ap[B](f: Maybe[A => B]): Maybe[B] = ???

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = ???

}

final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]
  

object Maybe {

  // Smart constructors
  def empty[A]: Maybe[A] = Empty[A]
  def just[A](a: A): Maybe[A] = Just(a)
  
}
