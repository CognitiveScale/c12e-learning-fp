package com.c12e.learn
package data

/* 
 * Scott encoding of Maybe
 */

//trait Bool {
  //import Bool.{f, t}
  //def matchit[B](ifTrue: => B, ifFalse: => B): B 
  //def not: Bool = matchit(f, t)
//}

//object Bool {
  //val t: Bool = new Bool { def matchit[B](ifTrue: => B, ifFalse: => B): B = ifTrue }
  //val f: Bool = new Bool { def matchit[B](ifTrue: => B, ifFalse: => B): B = ifFalse }
//}

trait M[A] {
  // match / visit
  //def matchit[B](b:B, f:A => B):B 
  def matchit[B]: (B, A => B) => B
  //def fold[B](ifN:B, ifJust:A => B):B = matchit(ifN, ifJust)
  def fold[B]: (B, A => B) => B = matchit 
  //def maybe[B](b:B, f:(A => B)):B = fold(b, f)
  def maybe[B]: (B, A => B) => B = fold 

  def isNothing: Boolean = matchit(true, (h => false))

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String = "M: " + fold[String]("Nothing", (v => "Just " + v.toString ))

}

object M {

  // method that takes a Maybe vs a method on Maybe
  def isNothing[A](maybe: M[A]): Boolean = maybe.matchit(true, (h => false))

  def nothing[A]: M[A] = new M[A]() { def matchit[B]: (B, A => B) => B = (b, _) => b }
  
  def just[A](v:A):M[A] = 
    new M[A]() { def matchit[B]: (B, A => B) => B = (_, f) => f(v)}
}


object TestM extends App {
  import M._

  val n:M[Int] = nothing
  val j:M[Int] = just(2)
  println(n)
  println(j)
  println(n.isNothing)
  println(isNothing(n))
  println(j.isNothing)
  println(j.maybe("default", x => "String " + x.toString))
  println(n.maybe("default", x => "String " + x.toString))
}
