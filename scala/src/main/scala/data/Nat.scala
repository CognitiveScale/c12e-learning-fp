package com.c12e.learn
package data


import Nat.{ zero, succ }


/** data Nat = Zero | Succ Nat */
sealed trait Nat {

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def fold[B](ifZero: B)(ifSucc: B => B): B = {
    this match {
      case Zero() => ifZero
      case Succ(s) => ifSucc(s.fold(ifZero)(ifSucc))
    }
  }

  // def +(n: Nat): Nat = ???

  // def *(n: Nat): Nat = ???

}

final case class Zero() extends Nat
final case class Succ(s: Nat) extends Nat


object Nat{

  def zero: Nat = Zero()
  def succ(s: Nat): Nat = Succ(s)

  def toInt(n: Nat): Int = n.fold(0) { _ + 1 }

  // def fromInt(i: Int): Nat = ???

}


object NatTest extends App {
  val n: Nat = succ(succ(zero))

  // TODO: write tests

}
