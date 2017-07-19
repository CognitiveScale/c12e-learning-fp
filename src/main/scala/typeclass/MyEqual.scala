package com.c12e.learn
package typeclass

import annotation.implicitNotFound

@implicitNotFound("No member of type class MyEqual in scope for ${T}")
trait MyEqual[T] {
  def equal(a: T, b: T): Boolean 
}

object MyEqual {
  def equal[T](a: T, b: T)(implicit ev: MyEqual[T]): Boolean = ev.equal(a, b)

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit object MyEqualInt extends MyEqual[Int] {
    def equal(a: Int, b: Int): Boolean = a.equals(b)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit object MyEqualString extends MyEqual[String] {
    def equal(a: String, b: String): Boolean = a.equals(b)
  }
}


object TestMyEqual extends App {
  import MyEqual._

  val n: Boolean = equal(3, 4)
  val s: Boolean = equal("Hello", "Hello")
  println("******************")
  println(n)
  println(s)
}
