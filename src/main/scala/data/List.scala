package com.c12e.learn
package data

sealed trait IList[A] {
  import IList.{nil, cons}

  def fold[B](ifNil: B)(ifCons: (A, B) => B): B = 
    this match {
      case Nil() => ifNil
      case Cons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
    }

  def foldL[B](ifNil: B)(ifCons: (A, B) => B): B =
    this match{
      case Nil() => ifNil
      case Cons(h, t) => {
        val current = ifCons(h, ifNil)
        t.foldL(current)(ifCons)
      }
    }

  def map[B](f: A => B): IList[B] = 
    fold(nil[B])((x, y) => cons(f(x), y))

  def ap[B](mf: IList[A => B]): IList[B] =  ???

  def flatMap[B](f: (A, IList[B]) => IList[B]): IList[B] = 
    fold(nil[B])(f)
    
  def reverse = foldL(nil[A])(cons)

  def maybeTailCase: Maybe[IList[A]] =
    this match {
      case Nil() => Maybe.empty[IList[A]]
      case Cons(h, t) => Maybe.just(t)
    }

  def maybeTailFold: Maybe[IList[A]] = {
    type Acc = (Maybe[IList[A]], Maybe[IList[A]])
    val ifNil: Acc = (Maybe.empty[IList[A]], Maybe.just(nil[A]))
    def ifCons(a:A, acc:Acc): Acc = (acc._2, acc._2 map { cons(a, _) })
    fold(ifNil)(ifCons)._1
  }
}

final case class Nil[A]() extends IList[A] { }
final case class Cons[A](head: A, tail: IList[A]) extends IList[A] { }

object IList {
  def nil[A]: IList[A] = Nil()
  def cons[A](head: A, tail: IList[A]): IList[A] = Cons[A](head, tail)
}

object MainList extends App{
  import IList.{nil, cons}
  val l: IList[Int] = cons(1, cons(2, cons(3, cons(4, nil))))
  print("list def:\t ")
  print(l)
  print("\nid:\t\t ")
  print(l.fold(nil[Int])(cons))
  print("\nreverse:\t ")
  print(l.reverse)
  print("\nmaybeTailCase:\t ")
  print(l.maybeTailCase)
  print("\nmap:\t\t ")
  print(l.map(x => x*2))
  print("\nflatMap:\t ")
  print(l.flatMap((h, t:IList[Int]) => cons(h*2, t)))
  print("\nmaybeTailFold:\t ")
  print(l.maybeTailFold)
}
