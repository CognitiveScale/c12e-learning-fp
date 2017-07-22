package com.c12e.learn
package typeclass

trait Plus[F[_]] {
  def plus[A](a1: F[A], a2: F[A]): F[A]
}

object Plus {

  def apply[F[_]](implicit ev: Plus[F]): Plus[F] = ev


  implicit def list: Plus[List] = 
    new Plus[List] {
      def plus[A](x: List[A], y: List[A]) = x ++ y
    }
  
  final class Ops[F[_], A](val a: F[A]) extends AnyVal {
    def <+>(a2: F[A])(implicit ev: Plus[F]): F[A] = ev.plus(a, a2)
  }

  trait Syntax {
    // TODO: how can we avoid these warts? 
    // can we rewrite the function so that these warts don't happen?
    @SuppressWarnings(Array("org.wartremover.warts.ExplicitImplicitTypes",
                            "org.wartremover.warts.ImplicitConversion",
                            "org.wartremover.warts.PublicInference"))
    implicit def plusToOps[F[_] : Plus, A](a: F[A]) = new Ops(a)
  }

  object Syntax extends Syntax
}


object TestPlus extends App {
  import Plus._
  import Plus.Syntax._

  println(List(1,2) <+> List(3,4))
}
