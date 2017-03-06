package com.c12e.learn

import scala.util.matching.Regex

final case class Parser[A](parse: String => Option[(A, String)]) extends AnyVal {

  def map[B](f: A => B): Parser[B] = {
    def g(cs: String): Option[(B, String)] = {
      parse(cs) match {
        case Some((a, str)) => Some((f(a), str))
        case None => None
      }
    }
    Parser(g)
  }
  
  def mapBetter[B](f: A => B): Parser[B] =
    Parser { parse(_).map { case (a, rest)  => (f(a), rest) } }

  def flatMap[B](f: A => Parser[B]): Parser[B] = 
    //Parser {str => parse(str).map { case (a, rest) => f(a).parse(rest)}.flatten}
    Parser {str => parse(str).flatMap { case (a, rest) => f(a).parse(rest)}}
  

  def ap[B](p: Parser[A => B]): Parser[B] = 
    //Parser {str => parse(str).map {case (a, rest) => p.parse(rest) }}
    for {
      a <- this
      f <- p
    } yield f(a)

  def orElse(p: Parser[A]): Parser[A] = 
    Parser { str => parse(str) orElse p.parse(str) }
        
  // regex +
  def oneOrMore: Parser[List[A]] = ???

  // regex *
  def zeroOrMore: Parser[List[A]] = ???
}

object Parser{
  def fail[A]: Parser[A] = Parser(_ => None)

  // pure / return  a -> f a
  def pass[A](a: A): Parser[A] = Parser(str => Some((a, str)))

  def regex(reg: Regex): Parser[String] = {
    def f(str: String): Option[(String, String)] = {
      str.head match {
        case reg(g) => Some((g.toString, str.tail))
        case _ => None
      }
    }
    Parser(f)  
  }

  def char: Parser[String] = regex("""(.)""".r)

  def digitStr: Parser[String] = regex("""([0-9])""".r)
  
  def alpha: Parser[String] = regex("""([a-zA-Z])""".r)

  def digitIntUgly: Parser[Int] = {
    def f(str: String): Option[(Int, String)] = {
      digitStr.parse(str) match {
        case Some((i, str_)) => Some((i.toInt, str_)) 
        case None => None
      }
    }
    Parser(f)
  }

  def digitInt: Parser[Int] = 
    digitStr.map(x => x.toInt)
    //char.flatMap { x => 
      //try { pass(x.toInt) } 
      //catch {
        //case (e : NumberFormatException) => fail
      //}
    //}
     
  val condParse: Parser[(Int, String)] =
//    digitInt.flatMap { i => (if (i == 3) digitStr else alpha).map { a => (i, a) } }
    for {
      i <- digitInt
      a  <- if (i == 3) digitStr else alpha
    } yield  (i, a)
}

object ParserApp extends App{

  import Parser._

  assert(fail.parse("A") == None)
  assert(pass("1bc").parse("Anything") == Some(("1bc", "Anything")))
  assert(regex("""([0-9])""".r).parse("1bc") == Some(("1", "bc")))

  assert(alpha.parse("1bc") == None)
  assert(alpha.parse("abc") == Some(("a", "bc")))
  
  assert(digitStr.parse("1bc") == Some(("1", "bc")))
  assert(digitStr.parse("abc") == None)

  assert(digitIntUgly.parse("1bc") == Some((1, "bc")))

  assert(digitInt.parse("abc") == None)
  assert(digitInt.parse("1bc") == Some((1, "bc")))

  assert(condParse.parse("0ab") == Some(((0,"a"), "b")))
  assert(condParse.parse("01b") == None)
  assert(condParse.parse("31b") == Some(((3,"1"), "b")))
  assert(condParse.parse("3ab") == None)

  assert(digitStr.orElse(alpha).parse("abc")  == Some(("a", "bc")))
  assert(digitStr.orElse(alpha).parse("0bc")  == Some(("0", "bc")))

}
