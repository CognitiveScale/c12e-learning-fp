/**
  * Created by akumar on 24/08/17.
  */

package com.c12e.learn
package data


sealed trait MaybeScott[A] {

  import MaybeScott.{empty, just}

  def imatch[B](b: B, f: A => B): B

  def fold[B](ifEmpty: B, ifJust: A => B): B =
    imatch(ifEmpty, value => ifJust(value))

  def map[B](f: A => B): MaybeScott[B] =
    fold(empty, f andThen just)

  def flatMap[B](f: A => MaybeScott[B]): MaybeScott[B] =
    fold(empty, f)

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def toString: String = fold[String]("empty", v => "Just " + v.toString)

}


object MaybeScott {

  def just[A](v: A): MaybeScott[A] =
    new MaybeScott[A] {
      override def imatch[B](b: B, f: A => B): B = f(v)
    }

  def empty[A]: MaybeScott[A] = new MaybeScott[A] {
    override def imatch[B](b: B, f: (A) => B): B = b
  }

}

object MaybeScottTest extends App {

  import MaybeScott.{empty, just}

  val x: MaybeScott[Int] = just(1)
  val y: MaybeScott[Int] = empty

  println(x)
  println(x.flatMap(x => just(x + 1)))

}