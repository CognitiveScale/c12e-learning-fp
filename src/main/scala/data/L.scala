package com.c12e.learn
package data

/* 
 * Scott encoding of List
 */

trait L[A] {
  type BiFunction[B] = (A => L[A] => B)

  def eval[B](b:B, f:BiFunction[B]):B 

  def isNil():Boolean = eval(true, (h => t => false))

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def foldRight[B](ifNil:B, ifCons:(A => B => B)):B = 
    eval(ifNil, h => t => ifCons(h)(t.foldRight(ifNil, ifCons)))

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String = "L: " + foldRight[String]("", (h => t => h.toString + " " + t))
}

object L {

  def nil[A]():L[A] = new L[A]() { def eval[B](b:B, f:BiFunction[B]): B = b }
  
  def cons[A](head:A, tail:L[A]):L[A] = 
    new L[A]() { def eval[B](b:B, f:BiFunction[B]): B = f(head)(tail)}
}


object TestL extends App {
  import L._
  val n:L[Int] = nil()
  val l:L[Int] = cons(1, cons(2, cons(3, nil()))) 
  println(n.isNil())
  println(l.isNil())
  println(l)
}
