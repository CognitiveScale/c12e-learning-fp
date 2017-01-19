package com.c12e.learn
package intro


trait PointI {
  def x: Int
  def y: Int
}

object PointI {

  def apply(_x: Int, _y: Int): PointI =
    new PointI {
      def x = _x
      def y = _y
    }

}


sealed abstract class PointADT {

  def fold[A](f: (Int, Int) => A): A =
    this match {
      case PointDC(x, y) => f(x, y)
    }

  def x: Int = fold((x, y) => x)
  def y: Int = fold((x, y) => y)

}

final case class PointDC(xVal: Int, yVal: Int) extends PointADT

object PointADT {
  def apply(x: Int, y: Int): PointADT = PointDC(x, y)
}


class PointC(val x: Int, val y: Int)

object PointC {
  def apply(x: Int, y: Int): PointC =
    new PointC(x, y)
}


final case class Point(x: Int, y: Int)


object AbstractionExample extends App {

  val pi = PointI(1,2)
  val padt = PointADT(1,2)
  val pc = PointC(1,2)
  val p = Point(1,2)

  println(pi.x)
  println(padt.x)
  println(pc.x)
  println(p.x)

}
