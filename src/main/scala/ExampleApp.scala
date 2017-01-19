package com.c12e.learn


import com.c12e.learn.data.Nel
import com.c12e.learn.data.\/.Syntax._
import com.c12e.learn.typeclass.Functor
import com.c12e.learn.typeclass.Semigroup
import com.c12e.learn.typeclass.Semigroup.Syntax._
import com.c12e.learn.stdlib.All._
import com.c12e.learn.stdlib.instances.StringInstances


object ExampleApp extends App {

  println(implicitly[Semigroup[String]].append("a", "b"))

  println(Semigroup[String].append("a", "b"))

  // silly, but possible (explicitly passing an implicit parameter)
  println(Semigroup.apply(StringInstances.stringMonoid).append("a", "b"))

  println("a" |+| "b")

  println(new Semigroup.Ops("a") |+| "b")

  println(Nel(1,2,3) |+| Nel(4,5,6))

  println(1.right[String])

  println(
    Functor[Int => ?]
      .map(1 + _) { i => s"[${i}]"}
      .apply(5))

}
