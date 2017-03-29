package com.c12e.learn


import scala.util.matching.Regex

import scalaz.MonadPlus

final case class Parser[A](parse: String => Option[(A, String)]) extends AnyVal {

  import Parser.pass

  def mapUgly[B](f: A => B): Parser[B] = {
    def g(cs: String): Option[(B, String)] = {
      parse(cs) match {
        case Some((a, str)) => Some((f(a), str))
        case None => None
      }
    }
    Parser(g)
  }
  
  def map[B](f: A => B): Parser[B] =
    Parser { parse(_).map { case (a, rest)  => (f(a), rest) } }

  def flatMap[B](f: A => Parser[B]): Parser[B] = 
    //Parser {str => parse(str).map { case (a, rest) => f(a).parse(rest)}.flatten}
    Parser {str => parse(str).flatMap { case (a, rest) => f(a).parse(rest)}}
  
  def ap[B](p: => Parser[A => B]): Parser[B] = 
    // version 1
    //Parser {str => parse(str).flatMap {case (a, rest) => p.parse(rest).map{case (f, r) =>  (f(a), r)}}}
    // version 2
    //for {
      //a <- this
      //f <- p
    //} yield f(a)
    // version 3
    flatMap { a => p.map { f => f(a) } }

  def orElse(p: => Parser[A]): Parser[A] = 
    Parser { str => parse(str) orElse p.parse(str) }
        
  // regex +
  def oneOrMore: Parser[List[A]] = {
    // version 1
    //for {
      //a <- this
      //as <- zeroOrMore
    //} yield (a :: as)
    // version 2
    flatMap { a => zeroOrMore.map { as => a :: as } }
    // version 3
    // ap(zeroOrMore.map{ as => a => a :: as })
  }

  // regex *
  def zeroOrMore: Parser[List[A]] = {
		oneOrMore.orElse(pass(List[A]()))
	}
}

object Parser{

  def fail[A]: Parser[A] = Parser(_ => None)

  // pure / return  a -> f a
  def pass[A](a: => A): Parser[A] = Parser(str => Some((a, str)))

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
     
  def floatStr: Parser[String] =
    //digitStr.flatMap { n => regex("""(\.)""".r).zeroOrMore.parse(rest)
    for {
      n <- digitStr.oneOrMore
      d <- regex("""(\.)""".r).zeroOrMore
      f <- digitStr.zeroOrMore
    } yield (n ++ d ++ f).mkString

  def floatStrPair: Parser[List[String]] =
    for {
      coord1 <- floatStr
      coma1 <- thisChar(',')
      coord2 <- floatStr
      coma2 <- thisChar(',').zeroOrMore //should be zeroOrOne
    } yield List(coord1, coord2)

  def thisChar(chr: Char): Parser[String] = {
    def f(str: String): Option[(String, String)] = {
      if (str.isEmpty) None
      else {
        str.head match {
          case `chr` => Some((chr.toString, str.tail))
          case _ => None
        }
      }
    }
    Parser(f)  
  }

  def thisStr(str: => String): Parser[String] = {
    if (str.isEmpty) pass("")
    else {
      for {
        a <- thisChar(str.head)
        b <- thisStr(str.tail)
      } yield (a ++ b) 
    }
  }

  def PD: Parser[List[String]] =  {
    for {
      head <- thisStr("PD")
      body <- floatStrPair.zeroOrMore
      close<- thisChar(';').zeroOrMore //zero or one
    // v1
    } yield ( List(head) ++ body.flatten)
    // v2
    //} yield ( List(head) ++ (body flatMap (x => x)))
  }


  val condParse: Parser[(Int, String)] =
//    digitInt.flatMap { i => (if (i == 3) digitStr else alpha).map { a => (i, a) } }
    for {
      i <- digitInt
      a  <- if (i == 3) digitStr else alpha
    } yield  (i, a)

  implicit val monadPlus: MonadPlus[Parser] =
    new MonadPlus[Parser] { 
      def point[A](a: => A): Parser[A] = pass(a)
      def bind[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = pa flatMap f
      def plus[A](pa: Parser[A], b: => Parser[A]): Parser[A] = pa orElse b
      def empty[A]: Parser[A] = fail
    }

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

  assert(digitInt.oneOrMore.parse("123abc") == Some((List(1,2,3), "abc")))

  assert(floatStr.parse("1.0abc") == Some(("1.0", "abc")))
  assert(floatStr.parse("1.32abc") == Some(("1.32", "abc")))
  assert(floatStr.parse("1abc") == Some(("1", "abc")))
  assert(floatStr.parse("10abc") == Some(("10", "abc")))
  assert(floatStr.parse("abc") == None)

  assert(thisChar('A').parse("ABC") == Some(("A", "BC")))
  assert(thisChar('A').parse("") == None)

  assert(thisStr("AB").parse("ABC123") == Some(("AB", "C123")))

  assert(floatStrPair.parse("1,2.3;PD") == Some((List("1", "2.3"), ";PD")))

  assert(PD.parse("PD;") == Some((List("PD"), "")))
  assert(PD.parse("PD1,2,3.2,4;") == Some((List("PD", "1", "2", "3.2", "4"), "")))

}
