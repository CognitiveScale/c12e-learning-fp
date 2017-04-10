package com.c12e.learn
package intro


object CurryExample extends App {

  def add(a: Int, b: Int): Int =
    a + b

  def addCurry(a: Int)(b: Int): Int =
    a + b

  def addCurryManual(a: Int): Int => Int =
     { b => a + b }

  val addHof: Int => Int => Int =
     { a => { b => a + b } }

  val addHofUncurried: (Int, Int) => Int =
    { (a, b) => a + b }

  val addHofUncurriedTupled: ((Int, Int)) => Int =
    { t => t._1 + t._2 }

  println(add(1,2))
  println(addCurry(1)(2))
  println(addCurryManual(1)(2))
  println(addHof(1)(2))
  println(addHofUncurried(1, 2))
  println(addHofUncurried.tupled((1, 2)))
  println(addHofUncurriedTupled((1, 2)))

  val addPartialOne: Int => Int = add(1, _: Int) //Partial application
  val addCurryOne: Int => Int = addCurry(1)_ // Eta expansion?
  val addCurryManualOne: Int => Int = addCurryManual(1)
  val addHofOne: Int => Int = addHof(1)

  println(addPartialOne(100))
  println(addCurryOne(100))
  println(addCurryManualOne(100))
  println(addHofOne(100))

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    ???

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    ???

  def flip[A, B, C](f: A => B => C): B => A => C =
    ???

  def curryHof[A, B, C]: ((A, B) => C) => (A => B => C) =
    ???

}
