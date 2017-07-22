package com.c12e.learn
package typeclass

import com.c12e.learn.typeclass.Plus.Syntax

trait Plus[F[_]] {
  def plus[A](a:F[A], b: F[A]): F[A]
}


object Plus {
  @inline def apply[F[_]](implicit ev: Plus[F]): Plus[F] = ev

  final class Ops[F[_],A](val a: F[A]) extends AnyVal {
    @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
    def <+>(b: F[A])(implicit ev: Plus[F]): F[A] = ev.plus(a, b)
  }

  trait Syntax {
    @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
    implicit def toPlusOps[F[_] : Plus, A](fa: F[A]): Ops[F, A] =
      new Ops(fa)
  }

  object Syntax extends Syntax

  trait Laws {

    import Syntax._
    import Equal.Syntax._

    def plusAssociativity[F[_] : Plus, A]
    (x: F[A], y: F[A], z: F[A])(implicit ev: Equal[F[A]]): Boolean =
      ((x <+> y) <+> z) === (x <+> (y <+> z))

  }

  object Laws extends Laws

  implicit def listIsPlus: Plus[List] = new Plus[List] {
    def plus[A](a:List[A], b: List[A]): List[A] = a ++ b
  }

  implicit def optionIsPlus: Plus[Option] = new Plus[Option] {
    def plus[A](a:Option[A], b: Option[A]): Option[A] = a orElse b
  }
}


object TestPlus extends App {
  import Syntax._

  import scalaz.std.option._
  import scalaz.std.anyVal.intInstance
  import scalaz.std.list._
  import scalaz.syntax.equal._

//  import Equal.Syntax._
//  private val none : Option[Int] = None
//  implicit def myIntEqual: Equal[Int] = Equal.fromObject[Int]
//  implicit def myOptionEqual[A: Equal]: Equal[Option[A]] = new Equal[Option[A]] {
//    override def equal(a1: Option[A], a2: Option[A])(implicit ev: Equal[A]): Boolean =
//      (a1,a2) match {
//        case (None,None) => true
//        case (Some(aa1),Some(aa2)) => (aa1 === aa2)
//        case _ => false
//      }
//  }
//  implicit def myListEqual[A: Equal]: Equal[List[A]] = new Equal[List[A]] {
//    override def equal(l1: List[A], l2: List[A])(implicit ev: Equal[A]): Boolean =
//      (l1, l2) match {
//        case (Nil,Nil) => true
//        case (h1 :: tail1, h2 :: tail2) => h1 === h2 && tail1 === tail2
//        case _ => false
//      }
//  }

  assert((List(1, 2, 3) <+> List(4, 5, 6)) === List(1, 2, 3, 4, 5, 6))
  assert((Option(1) <+> Option(2)) === Option(1))
  assert((none[Int] <+> Option(1)) === Option(1))
}