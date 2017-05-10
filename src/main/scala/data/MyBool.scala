package com.c12e.learn
package data


/** data MyBool = MyTrue | MyFalse */
sealed trait MyBool {

  import MyBool.{ myTrue, myFalse }

  // cata
  def fold[A](ifTrue: => A, ifFalse: => A): A =
    this match {
      case MyTrue() => ifTrue
      case MyFalse() => ifFalse
    }

  def not: MyBool = fold(myFalse, myTrue)

  def and(that: MyBool): MyBool = ???

  def or(that: MyBool): MyBool = ???

}

final case class MyTrue() extends MyBool
final case class MyFalse() extends MyBool


object MyBool {

  val myTrue: MyBool = MyTrue()

  val myFalse: MyBool = MyFalse()

  def fold[A](x: MyBool, ifTrue: => A, ifFalse: => A): A = ???

  def not(x: MyBool): MyBool = ???

  def and(x: MyBool, y: MyBool): MyBool = ???

  def or(x: MyBool, y: MyBool): MyBool = ???

}
