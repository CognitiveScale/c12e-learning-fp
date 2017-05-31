package com.c12e.learn
package data

sealed trait BCT[A] {

  def fold[B](ifNada: B, ifNodo: (A, B, B) => B): B = {
    this match {
      case Nada()         => ifNada
      case Nodo(v, l, r)  => ifNodo(v, l.fold(ifNada, ifNodo), r.fold(ifNada, ifNodo))
    }
  }
}

final case class Nada[A]() extends BCT[A]
final case class Nodo[A](v: A, l: BCT[A], r: BCT[A]) extends BCT[A]

object BCT {
  def nada[A](): BCT[A] = Nada()
  def nodo[A](v: A, l: BCT[A], r: BCT[A]): BCT[A] = Nodo(v, l, r)
}

object RunBinaryChristmasTree extends App {
  import BCT.{nada, nodo}

  val t21: BCT[Int] = nodo(21, nada(), nada())
  val t22: BCT[Int] = nodo(22, nada(), nada())
  val t31: BCT[Int] = nodo(31, t21, t22)
  val t32: BCT[Int] = nodo(32, nada(), nada())
  val top: BCT[Int] = nodo(4, t31, t32)
  print("Tree:\t")
  println(top)  

  val l: List[Int] = top.fold(List[Int](), 
                            (v: Int, x:List[Int], y:List[Int]) => List[Int](v) ++ x ++ y)
	println("List:\t" + l.toString())

  val s: Int = top.fold(0, (v: Int, x:Int, y:Int) => v + x + y)
  println("Sum:\t" + s.toString())

}
