package com.c12e.learn
package data


sealed trait ScottIList[A] {

  import ScottIList.{nil, cons}

  def sMatch[B](b: B)(f: (A, ScottIList[A]) => B): B

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def fold[B](ifNil: B)(ifCons: (A, B) => B): B = 
    sMatch(ifNil)((h, t) => ifCons(h, t.fold(ifNil)(ifCons)))

  def ++(l: ScottIList[A]): ScottIList[A] = fold(l)(cons)

  def map[B](f: A => B): ScottIList[B] =
    fold(nil[B])((h, t) => cons(f(h), t))

  def flatMap[B](f: A => ScottIList[B]): ScottIList[B] =
    fold(nil[B])((h, t) => f(h) ++ t)
  
  def flatten[B](implicit ev: A =:= ScottIList[B]): ScottIList[B] =
    flatMap(ev)
  
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String = "IList: " + fold[String]("")((h, t) => h.toString + " " + t)

}

object ScottIList {

  def nil[A]: ScottIList[A] = 
    new ScottIList[A] {
      def sMatch[B](b: B)(f: (A, ScottIList[A]) => B): B = b
    }
  
  def cons[A](head: A, tail: ScottIList[A]): ScottIList[A] = 
    new ScottIList[A] {
      def sMatch[B](b: B)(f: (A, ScottIList[A]) => B): B = f(head, tail)
    }
}

object ScottIListTest extends App {
 
   import ScottIList.{nil, cons}
 
   val emptyList: ScottIList[Int] = nil
   val list: ScottIList[Int] = cons(1, cons(2, cons(3, nil)))
 
   val sum: Int = list.fold(0)((x: Int, y: Int) => x + y)
 
 
   println(list)
   println(list.map(a => a + 1))
   println(list.flatMap(a => cons(a + 1, nil)))
   println(sum)
 }