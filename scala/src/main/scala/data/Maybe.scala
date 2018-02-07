package com.c12e.learn
package data

import Maybe.{just,empty}

sealed trait Maybe[A] {


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
}

final case class Just[A](fromJust: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

object Maybe {

  def just[A](a: A): Maybe[A] = Just(a)
  def empty[A]: Maybe[A] = Empty()
}

// TODO: implement Plus (from scalaz) and PlusOps
