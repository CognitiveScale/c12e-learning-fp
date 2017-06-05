package com.c12e.learn
package data


import Nat.{zero, succ, fromInt, toInt}


/** data Nat = Zero | Succ Nat */
sealed trait Nat {

  def fold[B](ifZero: B)(ifSucc: B => B): B = {
    this match {
      case Zero() => ifZero
      case Succ(s) => ifSucc(s.fold(ifZero)(ifSucc))
    }
  }

  def ++(n: Nat): Nat = this match {
    case Zero() => n
    case Succ(s) => Succ(s + n)
  }

  def +(n: Nat): Nat = fold(n)(Succ)

  def *(n: Nat): Nat = fold(zero)(_ + n)

  override def toString: String = toInt(this).toString

}

final case class Zero() extends Nat
final case class Succ(s: Nat) extends Nat


object Nat{

  def zero: Nat = Zero()
  def succ(s: Nat): Nat = Succ(s)

  def toInt(n: Nat): Int = n.fold(0) { _ + 1 }

  def fromInt(i: Int): Nat = i match {
    case 0 => zero
    case n => Succ(fromInt(n - 1))
  }

}


object NatTest extends App {

  val two: Nat = succ(succ(zero))
  assert(toInt(two).equals(2), "toInt fail")

  val three: Nat = fromInt(3)
  assert(three.equals(succ(succ(succ(zero)))), "fromInt fail")

  val five = two + three
  println(s"$two + $three = $five")
  assert((two + three).equals(five), "Addition fail")

  println(s"$two + $zero = ${two + zero}")
  assert((two + zero).equals(two), "Add zero fail")

  println(s"$zero + $two = ${zero + two}")
  assert((zero + two).equals(two), "Add zero fail")

  val fifteen: Nat = three * five
  println(s"$three * $five = $fifteen")
  assert((three * five).equals(fifteen), "Multiplication fail")

  println(s"$zero * ${fromInt(10)} = $zero")
  assert((zero * fromInt(10)).equals(zero), "Multiplication with zero fail")

  println(s"${fromInt(10)} * $zero = $zero")
  assert((fromInt(10) * zero).equals(zero),"Multiplication with zero fail")

}
