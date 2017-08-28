package com.c12e.learn
package data


sealed trait ScottMaybe[A] {

  import ScottMaybe.{just, empty}

  def sMatch[B](b: B)(f: A => B): B

	def fold[B](ifEmpty: B)(ifJust: A => B): B =
    sMatch(ifEmpty)(v => ifJust(v))

  def andThen[X, Y, Z](f: X => Y)(g: Y => Z): X => Z =
    x => g(f(x))

	def map[B](f: A => B): ScottMaybe[B] =
		fold(empty[B])(f andThen just)

	def flatMap[B](f: A => ScottMaybe[B]): ScottMaybe[B] =
		fold(empty[B])(f)
	
	@SuppressWarnings(Array("org.wartremover.warts.ToString"))
	override def toString: String = "Maybe: " + fold[String]("empty")(v => "just " + v.toString)
    
}

object ScottMaybe {

	def just[A](a: A): ScottMaybe[A] = 
		new ScottMaybe[A] {
			def sMatch[B](b: B)(f: A => B) = f(a)
		}

	def empty[A]: ScottMaybe[A] = 
		new ScottMaybe[A] {
			def sMatch[B](b: B)(f: A => B) = b
		}
}

object ScottMaybeTest extends App {

	import ScottMaybe.{just, empty}

	val j: ScottMaybe[Int] = just(42)
	val e: ScottMaybe[Int] = empty

	println(e)
	println(j)

	println(j.map(j => j + 1))
	println(j.flatMap(j => just(j + 1)))
}
