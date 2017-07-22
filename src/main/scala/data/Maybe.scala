package com.c12e.learn
package data

import com.c12e.learn.typeclass.Plus
import com.c12e.learn.typeclass.Plus.Syntax._

sealed trait Maybe[A] {

  import Maybe.{just,empty}

  def fold[B](ifEmpty: B, ifJust: A => B): B = 
    this match {
      case Empty() => ifEmpty
      case Just(a) =>  ifJust(a)
    }

  def map[B](f: A => B): Maybe[B] =
    fold(empty[B], f andThen just)
    //fold(empty[B], (just[B] _) compose f)

  def flatMap[B](f: A => Maybe[B]): Maybe[B] =
    fold(empty[B], f)

  def orElse(b: Maybe[A]): Maybe[A] =
    fold(b, just)

}

final case class Just[A](fromJust: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

object Maybe {


  def just[A](a: A): Maybe[A] = Just(a)
  def empty[A](): Maybe[A] = Empty()
  
  implicit def maybeToPlus: Plus[Maybe] = 
    new Plus[Maybe] {
      def plus[A](x: Maybe[A], y: Maybe[A]) = x.orElse(y)
    }
}

object TestMaybe extends App {
  import Maybe._
  println(empty().orElse(just(3)))
  println(empty() <+> just(3))
  println(just(2) <+> just(3))
}
