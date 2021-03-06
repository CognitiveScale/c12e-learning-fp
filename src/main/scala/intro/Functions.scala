package com.c12e.learn
package intro


class Functions

object Functions extends App {

  val selfReference: Functions.type = Functions

  val result: Int =
    com.c12e.learn.intro.Functions.add(1,2)

  def add(a: Int, b: Int): Int =
    a + b

  println(add(1,2))

  val addEtaExpanded: (Int, Int) => Int = add _

  println(addEtaExpanded(1,2))

  def autoEtaExpand(f : (Int, Int) => Int): (Int, Int) => Int =
    f

  println(autoEtaExpand(add)(1,2))
  // println(identity(add)(1,2))  // WONT WORK

  val addUncurried: (Int, Int) => Int =
    { (a, b) => a + b }

  println(addUncurried(1, 2))

  val addUncurriedTupled: ((Int, Int)) => Int =
    { t => t._1 + t._2 }

  println(addUncurriedTupled((1, 2)))
  println(addUncurried.tupled((1, 2)))

  def addCurryManual(a: Int): Int => Int =
    { b => a + b }

  println(addCurryManual(1)(2))

  def addCurrySyntax(a: Int)(b: Int): Int =
    a + b

  println(addCurrySyntax(1)(2))

  val addCurry: Int => Int => Int =
     { a => { b => a + b } }
     // { a => b => a + b }

  println(addCurry(1)(2))

  val addPartialOne: Int => Int = add(1, _)
  val addCurrySyntaxOne: Int => Int = addCurrySyntax(1)
  val addCurryManualOne: Int => Int = addCurryManual(1)
  val addCurryOne: Int => Int = addCurry(1)

  println(addPartialOne(100))
  println(addCurrySyntaxOne(100))
  println(addCurryManualOne(100))
  println(addCurryOne(100))

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    ???

  def curryHof[A, B, C]: ((A, B) => C) => (A => B => C) =
    ???

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    ???

  def flip[A, B, C](f: A => B => C): B => A => C =
    ???

}
