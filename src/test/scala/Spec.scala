package com.c12e.learn
package test


import scala.reflect.ClassTag

import org.scalacheck._
import org.scalacheck.Prop.Result
import org.scalacheck.Gen.Parameters

import com.c12e.learn.typeclass.Equal
import com.c12e.learn.stdlib.instances.BooleanInstances._


abstract class Spec(initContext: String)
    extends Properties(initContext)
    with ArbitraryInstances {

  def checkAll(name: String, props: Properties) = {
    for ((name2, prop) <- props.properties) yield {
      property(name + ":" + name2) = prop
    }
  }

  def checkAll(props: Properties) = {
    for ((name, prop) <- props.properties) yield {
      property(name) = prop
    }
  }

  class PropertyOps(props: Properties) {
    def withProp(propName: String, prop: Prop) = new Properties(props.name) {
      for {(name, p) <- props.properties} property(name) = p
      property(propName) = prop
    }
  }

  implicit def enrichProperties(props: Properties) = new PropertyOps(props)

  private var context: String = ""

  class StringOps(s: String) {

    def should[A](a: => Any) = {
      val saved = context
      context = s
      try a finally context = saved
    }

    def in[A](a: => A)(implicit ev: (A) => Prop) =
      property(context + ":" + s) = Prop { prms => ev(a).apply(prms) }

  }

  implicit def enrichString(s: String) = new StringOps(s)

  def check(x: => Boolean): Prop = {
    x must_===(true)
  }

  def fail(msg: String): Nothing = throw new AssertionError(msg)

  class AnyOps[A](actual: => A) {

    def must_===
        (expected: A)(implicit equal: Equal[A]): Unit = {
      val act = actual
      def test = Equal[A].equal(expected, act)
      def koMessage = s"${act} !== ${expected}"
      if (!test)
        fail(koMessage)
    }

    def mustMatch(f: PartialFunction[A, Boolean]): Unit = {
      val act = actual
      def test = f.isDefinedAt(act) && f(act)
      def koMessage = s"${act} does not satisfy partial function"
      if (!test)
        fail(koMessage)
    }

    def and[B](b: => B): B = {
      actual
      b
    }

    def mustBe_<(x: Int)(implicit ev: A <:< Int) = {
      val act = actual
      def test = ev(act) < x
      def koMessage = s"${actual} <! ${x}"
      if (!test)
        fail(koMessage)
    }

    def mustThrowA[T <: Throwable](implicit tag: ClassTag[T]): Unit = {
      val erasedClass = tag.runtimeClass
      try {
        actual
        fail("no exception thrown, expected " + erasedClass)
      } catch {
        case ex: Throwable =>
          if (!erasedClass.isInstance(ex))
            fail("wrong exception thrown, expected: " + erasedClass + " got: " + ex)
      }
    }
  }

  implicit def enrichAny[A](actual: => A): AnyOps[A] = new AnyOps(actual)

  def prop[T, R]
      (result: T => R)
      (implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T])
      : Prop =
    check1(result)

  implicit def propToProp(p: => Prop): Prop = p

  implicit def check1[T, R]
      (result: T => R)
      (implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T])
      : Prop =
    Prop.forAll((t: T) => toProp(result(t)))

  implicit def booleanToProp(b: => Boolean): Prop = Prop.secure(b)

  implicit def unitToProp(u: => Unit): Prop = booleanToProp({u; true})

  implicit def unitToProp2(u: Unit): Prop = booleanToProp(true)

}
