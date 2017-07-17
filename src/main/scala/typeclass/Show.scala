package com.c12e.learn
package typeclass

import annotation.implicitNotFound

@implicitNotFound("No member of type class Show in scope for ${T}")
trait Show[T] {
  def show(t: T): String
}

object Show {
  def show[T](t: T)(implicit ev: Show[T]): String = ev.show(t)

  implicit object ShowableInt extends Show[Int] {
    def show(n: Int): String = "Int: ~~~ " + n.toString() + " ~~~~"
  }

  implicit object ShowableString extends Show[String] {
    def show(n: String): String = {
      "String: --- " + n + " ---"
    }
  }
}

class ShowOps[A](val a: A) extends AnyVal {
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def >->(b: A)(implicit ev: Show[A]): String = ev.show(a) + " >-> " + ev.show(b)
}

trait ShowSyntax {
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
  implicit def toShowOps[A : Show](a: A): ShowOps[A] = new ShowOps(a)
}

object ShowSyntax extends ShowSyntax

// TEST IT ------------------

object TestShow extends App {
  import Show._
  import ShowSyntax._

  val n: Int = 4
  val s: String = "hello"
  println(show(n))
  println(show(s))
  println("a" >-> "b")
  println(1 >-> 2)
}
