package data

import com.c12e.learn.typeclass.Equal
import Equal.Syntax._

sealed trait ScottMaybe[A] {
  import ScottMaybe.{just,nothing}

  // is this still scott encoding, or does it have to be (c1, c2, c3...) => B?
  def matchS[B]: B => (A => B) => B

  def fold[B]: B => (A => B) => B = matchS

  def getOrElse: A => A = matchS(_)(identity[A])

  def map[B]: (A => B) => ScottMaybe[B] = f => matchS(nothing[B])(just compose f)

  def flatMap[B]: (A => ScottMaybe[B]) => ScottMaybe[B] = matchS(nothing[B])(_)

  def isNothing: Boolean = matchS(true)(_ => false)

  def filter: (A => Boolean) => ScottMaybe[A] = f => matchS(nothing[A])(a => if(f(a)) this else nothing[A])
  //def filter: (A => Boolean) => ScottMaybe[A] = f => map(f).flatMap(b => if(b) this else nothing)

}

object ScottMaybe {

  implicit def intEqual: Equal[Int] = Equal.fromObject[Int]
  implicit def scottMaybeEqual[A: Equal]: Equal[ScottMaybe[A]] = new Equal[ScottMaybe[A]] {
    def equal(m1: ScottMaybe[A], m2: ScottMaybe[A]): Boolean =
      m1.matchS(m2.isNothing)(a1 => m2.matchS(false)(_ === a1))
  }

  def nothing[A]: ScottMaybe[A] = new ScottMaybe[A] {
    def matchS[B]: B => (A => B) => B = b => _ => b
  }
  def just[A]: A => ScottMaybe[A] = a => new ScottMaybe[A] {
    def matchS[B]: B => (A => B) => B = _ => f => f(a)
  }

}

object TestPlus extends App {
  import ScottMaybe._

  assert(just(1) === just(1))
  assert(nothing[Int] === nothing)
  assert(!(nothing[Int] === just(1)))
  assert(!(just(1) === nothing))

  assert(!just(1).isNothing)
  assert(nothing[Int].isNothing)

  assert(just(1).getOrElse(3) === 1)
  assert(nothing.getOrElse(3) === 3)

  assert(just(1).fold(5)(_ * 2) === 2)
  assert(nothing[Int].fold(5)(_ * 2) === 5)

  assert(just(1).map(_ * 2) === just(2))
  assert(nothing[Int].map(_ * 2) === nothing)

  def getIfEven: Int => ScottMaybe[Int] = x => if (x % 2 === 0) just(x) else nothing
  assert(just(3).flatMap(getIfEven).isNothing)
  assert(just(4).flatMap(getIfEven) === just(4))

  def moreThan4: Int => Boolean = _ > 4
  assert(just(4).filter(moreThan4) === nothing)
  assert(just(5).filter(moreThan4) === just(5))

}