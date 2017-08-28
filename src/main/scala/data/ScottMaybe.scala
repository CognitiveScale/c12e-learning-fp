package com.c12e.learn
package data

import com.c12e.learn.typeclass.Equal
import Equal.Syntax._
/**
  * Created by mayankasthana on 8/28/17.
  */

sealed trait ScottMaybe[A] {
  import ScottMaybe.{nothing, just}

  def iMatch[B]: B => (A => B) => B

  def fold[B]: B => (A => B) => B = iMatch

  def map[B](f: A => B): ScottMaybe[B] = iMatch(nothing[B])(a => just(f(a)))

  def flatMap[B]: (A => ScottMaybe[B]) => ScottMaybe[B] = iMatch(nothing[B])(_)

  def filter: (A => Boolean) => ScottMaybe[A] = f => iMatch(nothing[A])(a => if(f(a)) this else nothing[A])

  def isNothing: Boolean = iMatch(true)(_ => false)

  def isJust: Boolean = !isNothing

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String = iMatch("Nothing")(_.toString)
}

object ScottMaybe {

  def nothing[A]: ScottMaybe[A] = new ScottMaybe[A] {
    def iMatch[B]: B => (A => B) => B = ifNothing => (_ => ifNothing)
  }

  def just[A]: A => ScottMaybe[A] = a => new ScottMaybe[A] {
    def iMatch[B]: B => (A => B) => B = _ => ifJust => ifJust(a)
  }

  implicit def scottMaybeEqual[A: Equal]: Equal[ScottMaybe[A]] = new Equal[ScottMaybe[A]] {
    override def equal(a1: ScottMaybe[A], a2: ScottMaybe[A]): Boolean =
      a1.iMatch(a2.isNothing)(aVal =>  a2.iMatch(false)(_ === aVal))
  }

  implicit def strEqual: Equal[String] = Equal.fromObject

  implicit def IntEqual: Equal[Int] = Equal.fromObject
}

object TestScottMaybe extends App {

  import ScottMaybe._

  assert(just("Hello") =/= just("World"))
  assert(just("Hello") === just("Hello"))
  assert(nothing[String] === nothing)

  assert(just("hello").map(st => st.toUpperCase()) === just("HELLO"))

  def hasSpace: String => Boolean = _.indexOf(' ') =/= -1
  assert(just("Scala is ok.").filter(hasSpace).isJust)
  assert(just("Look,nospaces!").filter(hasSpace).isNothing)

  assert(nothing[Int].map(_ * 2) === nothing)


}
