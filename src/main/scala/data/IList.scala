package data

/**
  * Created by cdraper on 6/8/17.
  */
sealed trait IList[A] {
  import IList._
  def fold[B](ifNil: B)(ifCons: (A, B) => B): B = this match {
    case Nil() => ifNil
    case Cons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
  }


  def map[B](f: A => B): IList[B] = fold(nil[B])((a, b) => Cons(f(a), b))

//  def flatMap[B](f: A => IList[B]): IList[B] = fold(nil[B])((a, bs) => f(a).fold(bs)((bb, bbs) => (Cons(bb, bbs))))

  // factor out append for nicer flatmap
  def append(as: IList[A]): IList[A] = fold(as)((a, aas) => Cons(a, aas))
  def flatMap[B](f: A => IList[B]): IList[B] = fold(nil[B])((a, bs) => f(a) append bs)

  // So, what are pros and cons of my three flattens?
  def flattenMe[X](implicit ev: A => IList[X]): IList[X] = flatMap[X](ev)

  // fumble thru reverse using fold
  def reverse: IList[A] = fold[IList[A] => IList[A]](identity)((a, accFn) => l => accFn(cons(a,l)))(nil)

  // then foldLeft isnt a big jump
  def foldLeftViaFoldRight[B](ifNil: B)(ifCons: (B, A) => B) : B =
    fold[B => B](identity)((a, accFn) => l => accFn(ifCons(l, a)))(ifNil)

  // for grins, what if we did foldRight via foldLeft via...
  def foldRightViaFoldLeft[B](ifNil: B)(ifCons: (A, B) => B) : B =
    foldLeftViaFoldRight[B => B](identity)((accFn, a) => l => accFn(ifCons(a, l)))(ifNil)

}

final case class Nil[A]() extends IList[A]
final case class Cons[A](head: A, tail: IList[A]) extends IList[A]

object IList {
  def nil[A]: IList[A] = Nil()
  def cons[A](head:A, tail: IList[A]): IList[A] = Cons(head, tail)

  // Try flatten two ways
  def flatten[A](l: IList[IList[A]]): IList[A] = l.flatMap(identity)
  def flatten2[A, X](l: IList[A])(implicit ev: A => IList[X]): IList[X] = l.flatMap[X](ev)

  // let's see if we can use the implicit with standard Lists
  implicit def listIsIList[X] (l: List[X]): IList[X] = l.foldRight(nil[X])(cons)
}

object RunIList {
  import IList._
  def main(args: Array[String]): Unit = {
    val list1 = Cons(1, cons(2, cons(3, nil)))
    val list2 = Cons(1, nil)
    assert(list1.head == 1)
    assert(list1.fold(5)((a: Int, b) => a + b) == 11)
    assert(list1.map[Int](_ + 2) == Cons(3, Cons(4, Cons(5, nil))))
    assert(list1.flatMap[Int](a => Cons(a + 1, Cons(a + 2, nil))) == Cons(2, Cons(3, Cons(3, Cons(4, Cons(4, Cons(5, nil)))))))
    assert(list1.append(cons(4, nil)) == cons(1, cons(2, cons(3, (cons(4, nil))))))
    val listOfList = cons(list1, Cons(list2, nil[IList[Int]]))
    val flattenedList = cons(1, cons(2, cons(3, (cons(1, nil)))))
    assert(flatten(listOfList) == flattenedList)
    assert(listOfList.flattenMe == flattenedList)
    assert(flatten2(listOfList) == flattenedList)
    assert(IList.flatten2(List(List(1, 2), List(3, 1))) == flattenedList)
    // should not compile
    // list1.flattenMe
    assert(list1.reverse == Cons(3, cons(2, cons(1, nil))))

    def multWithPrevious(i: Int, li: IList[Int]): IList[Int] = (i, li) match {
      case (_, Nil()) => cons(i, nil)
      case (_, Cons(h, _)) => cons(i * h, li)
    }
    assert(list1.fold(nil[Int])(multWithPrevious) == cons(6, cons(6, cons(3, nil))))

    assert(list1.foldLeftViaFoldRight(nil[Int])((i, l) => multWithPrevious(l, i)) == cons(6, cons(2, cons(1, nil))))
    assert(list1.foldRightViaFoldLeft(nil[Int])(multWithPrevious) == cons(6, cons(6, cons(3, nil))))

  }
}