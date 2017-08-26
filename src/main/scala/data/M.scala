package com.c12e.learn
package data

/* 
 * Scott encoding of Maybe
 */

trait M[A] {
  def eval[B](b:B, f:(A => B)):B 

  def foldRight[B](ifN:B, ifJust:(A => B)):B = 
    eval(ifN, v => ifJust(v))

  def isNothing():Boolean = eval(true, (h => false))

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String = "M: " + foldRight[String]("Nothing", (v => "Just " + v.toString ))

  def maybe[B](b:B, f:(A => B)):B = foldRight(b, f)
}

object M {

  def nothing[A]():M[A] = new M[A]() { def eval[B](b:B, f:(A => B)): B = b }
  
  def just[A](v:A):M[A] = 
    new M[A]() { def eval[B](b:B, f:(A => B)):B = f(v)}
}


object TestM extends App {
  import M._
  val n:M[Int] = nothing()
  val j:M[Int] = just(2)
  println(n)
  println(j)
  println(n.isNothing())
  println(j.isNothing())
  println(j.maybe("default", x => "String " + x.toString))
  println(n.maybe("default", x => "String " + x.toString))
}
