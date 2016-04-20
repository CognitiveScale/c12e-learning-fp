package com.c12e.learn

// could have done:
// import IList.{INil, ICons} -> brings this in scope for the file, not the package...
// and moved INil and ICons into companion object,

// Algebraic Data Type  - 80s, 90s - objects were compared to Abstract Data Types
// class Foooooooo(/*private members*/ private: Any)
// case class Foooooooooo2(/*private members*/ public_with_setters_and_getters: Any)


// binary compatibility - use abstract class instead of trait

// sealsed = all implementation needs to be in file  = get algebra because of this
// algebraic data types are "encoded" in scala with subtyping
// encoded -

// scala doesnt have algebraic data types be default - it has stuff that lets us simulate them
// sealed and case let us do that...

// Dont give too much semantic value to a type parameter - limits understanding of it
// Type parameters dont need to be one letter: A, B, C, can be words, but should easily be able to know whats a type and type param, thus
// dont use similar names for types and type parameters
// wont allow numbers....

sealed abstract class IList[A] {
    // Stuff here is a method, this is set
    // If it compiles, high prob its right, programming at a high level of abstraction

    // FOLD: destructure any object, and call function on held data.
    // can make fold for any data structure
    // reduce - fold makes sense on empty list, reduce doesnt,
    // reduce very similar to fold right
    // def fold[B](ifNil: B, ifCons: (A, B) => B): B (Non Curried)
    def fold[B](ifNil: B)(ifCons: (A, B) => B): B =  // (Curried)
        this match {
            case INil() => ifNil

            case ICons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
        }

    // Symbolid names that end in colon
    def +:(a: A): IList[A] = ICons(a, this)
    // because the name ends with a colon, it flips the element
    // x +: ilist
    // defined on the ilist, not the element, but this will call +: on the ilist!
    // gave us special syntax for all data structures ...
    // can also do ilist.+:(x)


    // To not blow stack....
    // IDE will tell you if tail call optimized
    // @annotation.tailrec
    // The last guy (recursive) has to be the call itself, and everything else is inputs
    def ++ (that: IList[A]): IList[A] =
        this match {
            case INil() => INil()
            // Binds x to that pattern match
            case x@ICons(h, t) => ICons(h, t ++ that)
            // Can also do double
            // case x@ICons(h, y@ICons(ht, t)) => ICons(h, t ++ that)
        }

    // maps are structure preserving, lenght of first map = length of second
    // dont need to write unit test for this, prooven for free with paramatricity
    // based on laws of functors, we know that length cant change
    def map[B](f: A => B): IList[B] =
      fold(INil(): IList[B])( (a, bs) => ICons(f(a), bs))

}

object Test {
    def main(x: Array[String]): Unit = {
        // import com.c12e.learn._ // _ => everything
        val a = ICons(1, ICons(2, INil()))
        val b = ICons(3, ICons(4, INil()))
        val c = ICons(5, ICons(6, INil()))

        // Proving associatvity
        println(a ++ b ++ c) // => Left associatvity is prefered)
        println((a ++ b) ++ c)
        println(a ++ (b ++ c))
        println((a ++ b).++(c))
        println(a.++(b.++(c)))

        // WHY THOUGH -> because we want to make a semigroup for list...

        //// Where would we put the implicit for semigourp implementation of Ilist
        // could put it on semigroup
        // but better / easier to manage if on semigroup of IList
        //type class should have no idea what its implementations are

    }
}



// why they are called algebras:
// limited
// sums of products

final case class INil[A] () extends IList[A]
final case class ICons[A] (head: A, tail: IList[A]) extends IList[A]

// Algebraic Algebra - definition of structure is definition of object
// Linked list -> pattern matching makes sense
// Set on other hand, no algebra to really represnet it, just represents memebership, pattern matching doesnt really apply

// Alternative to ADTs is interfaces

// if you subtype a case class, breaks some autogenerated methods, thus they have to be final
// case class makes pattern matching work

// similar to haskell's:
// data IList a = INil | ICons a (IList a)

// to infer that the data constuctor is not a type:
// typeclasses will look for implicits on companion objects
object IList {
    // Makes Inil look like a list
    // Inil is a constructor, not a type
    // type of A is now Seq
    // DONT USE SEQ in normal code, except here...
    def apply[A](a: A*): IList[A] =
        a.foldRight(nil[A])(cons)

        // Could have done fold left as well...
        // 1 2 3 4
        // 1 + (2 + (3 + (4 + 0))) -> Fold Left
        // ((((0 + 1) + 2) + 3) + 4)
        // Fold left would have been in reverse

        // Could implement foldleft in terms of foldRight and foldright in terms of foldleft

    def nil[A]: IList[A] = INil()
    def cons[A](h: A, t: IList[A]): IList[A] = ICons(h, t)

    // folds yield id func when passed constructors
    def id[A](list: IList[A]): IList[A] =
        // if its not curried, need [A] on both
        // list.fold(nil[A], cons[A])
        list.fold(nil[A])(cons)

        // Note: when you curry you add more stuff on heap (intermediary functions i guess) so it might not be wise to curry everything
        // Note: currying allows us to do this: (this is because if a funciton takes only one arg in scala, we dont need to use the () to call it)
        // list.fold(nil[A]) {
        //     ... define function
        // }

    // lazy vals have syncrounous costs ....
    // can deadlock on mutual dead vals
    // DONT USE lazy vals unless you need to!
    // Hard to think about whats going on - against FP - has caching element / side effectish

    // Fails - b is not defined when a is being evaluated
    // val a: Int = b + 1
    // def b = 2

    // Fine: funcitons evaluated when functions are first called, thus they are both there.
    // def a: Int = b + 1
    // def b: Int = 1

    // When you strictly evaluate a value, needs everything to be defined
    // When you stircticy evaluate a funciton - evaluated when first called...

    // difference between def and val - anything that doesnt take a paramter can be interchanges
    // it will be safe -

    // @style if defined as a trait,
    // .. can put implicits on traist
    // if mixinin is involved, give it unique name across mixins.

    // implicits -> an implications..
    // by labling things as implicits - candidates for universal truths -
    // the number of functions that turn tings into things are huge - the
    // an implicit implies that this should be a universal truth
    // Some blog article about univeral coherence
    // ... dude doesnt make advocay on global uniqueness, scalaers dont really talk about it, but haskellers do
    // if we ever import implicits, dont need to think of concepts

    // global uniqueness -> doesnt matter who wrote a function with that signature

    implicit def ilistSemigroup[A]: Semigroup[IList[A]] = {
        new Semigroup[IList[A]] {
            def append(a: IList[A], b: IList[A]): IList[A] = a ++ b
        }
    }

    implicit val ilistFunctor: Functor[IList] =
      // IList[A] full type, functor doesnt take type, it takes type constructor
      // List has a kind * -> *
      // Type of type is kind, List :: * -> *
      // List takes one type param before it gets a concrete type
      // Int :: *
      // List[Int] :: *
      // Int is a type, List is a type constructor
      // Functors take type constructors
      // ... so we need Functor[IList] ... but what function do we pass to the map now?
      // ... need to turn it into a val

      // number of _  equals number of * in *->*
      // Can have Foo[_, _[_,_]]
      // Formal notation for a * -> (*->*->*->) -> *
      // Function that takes types and spits out types - type constrictor
      // gamma, beta type constructors ? some type constructors can take a value and spit out types

      new Functor[IList] {
        def map(l1: IList)(f: A => B) = l1.map(f)
      }
      // prefidate functor logic - logic without varaibles?
}

// every fold,
// if you fold on your constructors, you get the original object back

// import ._  imports everything, where you use a value of IList, you get the value
// Where you use the type of IList, you get the type


// ... foldable doest have laws - have to concieve stuff, should have law above.

// // Wont work, gun should be different for every different a
// sealed trait SonOf[A]
// final case class something[A] extends SonOf[A]
// final case class gun() extends SonOf[A]
//
// // Need to make a covariant for this to work:
// sealed trait SonOf[+A]
// final case class something[A] extends SonOf[A]
// final case class gun() extends SonOf[Nothing]

// unapply meant for adts
// custom extractors - generally wrong way to think of design, master adts before we can custom make an unapply
// Using language features in clever way - not needed, use them how they were designed
// case and case classes designed for pattern matching

// console >>
//     import com.c12e.learning
//
//     INil[String]
//       => com.c12e.learn.INil[String]
//       com.c12e.learn.INil is not a type - INil should return an IList not an INil, if make case classes private, cant use them for pattern matching, but wont accidentally use data constructors as type s
// 	   IList.nil
//       => com.c12e.learn.IList[Nothing] = INil()


// in scala - memebers of a class are private
// in scala - memebers of a case class are public
// abstract - means hidden - info you dont know...
// abstract data types - really deep into the origin of CS

// algebraic data types

// Two possible places to put methods, on abstract class, or companion object
