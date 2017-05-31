Review
======

This document merely captures high-level topics discussed as we navigate the
accompanying code base.

It's intended to just be a review, rather than a stand-alone tutorial.


SBT Overview
------------

- The [sbt-extras][SBTEXTRAS] has a simple Bash script that bootstraps
  everything you need to develop in Scala assuming you

    - have a Java Runtime Environment (JRE) installed

    - have bash on your PATH.

- For CognitiveScale, we use a company-internal SBT plugin
  [c12e-sbt-plugin][SBTPLUGIN] to help make projects uniform.  One of the
  things it does is turn on some static analysis, relying heavily on another
  plugin called [Wartremover][WARTREMOVER].

- Please consider reading the official [SBT documentation][SBTDOCS].

- Know how to use common SBT tasks (compile, test, run, etc.).

- Know how to use the `~` to run any command in a "file-watch" mode.


First-time Reading/Navigating Scala
-----------------------------------

- Especially for non-Java programmers, know why code is put in the following
  directories:

    - src/main/scala
    - src/main/resources
    - src/test/scala
    - src/test/resources

- Understand the rational behind [chained package clauses][CHAINEDPKGS] in
  Scala.

- The folder hierarchy doesn't have to perfectly match package hierarchy
  (sometimes we don't).

- In the common case you have

    - an abstraction (class or trait) in a file with the same name
    - a companion object in the same file.

- One way objects are used are very similar to packages.  They organize types
  and functions related to a specific type.

- In case you're curious, one motivation for not fully unifying objects with
  packages in Scala is for more direct two-way interopt with Java code (they
  wouldn't be unified in the compiled bytecode ultimately).

- Types and values have different namespaces.

- Traits and classes are types.

- Objects are singleton values, not types.


Shifting to a Type Theoretic Viewpoint
--------------------------------------

- In set theory, different sets can have elements of different values.

- In set theory, elements can be members of different sets.

- In an intrinsic type theory, a value does not have any meaning until it is
  coupled with a type. And a value only carries one type at a time.

- This type theoretic viewpoint can help us avoid defects like confusing a "1"
  that's an integer from a "1" that's a IEEE floating point number.

- This type theoretic viewpoint can also help type inference (no combinatorial
  explosion of possible types as you evaluate the type of an expression).

- To start with, think of types as disjoint. The connections between types are
  just functions that map the values of one type to the values of another (or
  the same one).

- There is also a value ⊥ (pronounced "bottom") that in practice refer to these
  conditions of a program:

    - errors (no value to return)
    - non-termination (not returning)

- ⊥ causes problems in the consistency of our corresponding type theory, so we
  avoid both throwing errors and non-termination in our programming.

- A note, because in functional programming, a program is a value, there's a
  difference between evaluating an expression whose type is a program, and
  running it. This allows us to reason about a terminating algorithm to build a
  program that when run does not terminate (like a web server).


Function Programming is Programming with (Mathematical) Functions
-----------------------------------------------------------------

- Functions map values from a domain (input type) to a codomain (output type),
  such that if f(a) = b and f(a) = c, then b = c (only one value can be
  returned).

- Total functions don't return ⊥.

    - always terminate

    - return with no exception/error a value from the codomain for every
      possible value input from the domain.

- If a function is not total, it is called partial.

- In functional programming, we try to only use total functions, because ⊥ for
  the most part is just a bug/crash (and breaks the soundness of our type
  theory).

- One benefit of functional programming is that we can always rely on
  "beta-equivalence" -- that if an expression evaluates to a value, then all
  instances of that expression in the program can be replaced without changing
  the semantic of the expression. This is similar to being able to replace "1 +
  1" with "2" any place you see it in a mathematical expression -- it's always
  safe to do so (unless you're using some kind of weird notation).

- People often call property of having valid "beta-equivalence" "referential
  transparency," which is a term co-opted from the linguistics community by the
  programming languages community.

- From the programmer perspective, beta-equivalence means we can richly
  refactor code without worrying about accidentally changing the semantics of
  our program.

- We lose beta-equivalence when we use things like:

    - side-effects

    - reflection (actually, also a side-effect)

- Function composition is our gateway to modularity.  If we have (A => B) and
  (B => C), then we have (A => C) by composition.

- On top of using total functions, we'll also advocate for the following
  (discussed later):

    - large degree of parametricity

    - type-checking by the compiler

    - a more expressive type system (rich algebraic data types).


Good OO Leads to Good FP
------------------------

By writing tiny functions, we meet exactly the same design sentiments we wanted
with OO, and actually take them much further than we ever could with
traditional OO techniques. FP can be viewed less as a paradigm shift, and more
of a paradigm extension.

- Abstractions should have only one reason to be modified (single
  responsibility).

- Abstractions should be open to extension, but closed to modification.

- Favor composition over inheritance.  We actively avoid subtyping because:

    - getting subtypes right with respect to what's often called the "Liskov
      substitution property" is non-trivial.  For instance, is a Square a
      Rectangle?

    - variance causes head-aches in abstractions (`List#contains` takes an
      `Any` because of variance rules, but that can lead to defects)

    - we'll subsume the benefits of subtyping with simple functions (subtyping
      is kind of like function (`Subtype => Supertype`) that the compiler calls
      on your behalf.

- Favor immutability.

- Interfaces should be small and composable.


Intro to Algebras
-----------------

- What is an algebra?

    - things (what we have an algebra on)
    - operations (ways to turn things into other things)
    - laws (truths we can say are universal for the operations and things)

- Some important laws
    - commutativity (relates to concurrency)
    - associativity (relates to parallelism)
    - idempotency (relates to repeatable processing)
    - example libraries (Algebird, Spire, Scalaz, Cats)

- Semigroup[A]
    - things: A
    - operations:
        - mappend: A => A => A
    - laws:
        - mappend is associative

- Monoid[A]
    - extends Semigroup[A]
    - operations:
        - mappend: A => A => A
        - mempty: A
    - laws:
        - mappend is still associative
        - mempty is a right identity
        - mempty is a left identity

- Examples of algebras

    - Group of Bed Flips
    - Semigroup and Monoid of appending over Strings
    - Semigroup and Monoid of addition over Ints
    - Semigroup and Monoid of multiplication over Ints
    - Semigroup (and Monoid) of maximum over (bounded) Ints
    - Semigroup (and Monoid) of minimum over (bounded) Ints
    - Semigroup and Monoid of conjunction over Booleans
    - Semigroup and Monoid of disjunction over Booleans


Algebraic Data Types
--------------------

- Just as much as we can have algebras over values, we can also have algebras
  over types, which leads to Algebraic Data Types (abbreviated ADT, but not to
  be confused with "abstract data type").

- For the algebra of ADTs, The "things" will be types, and the "operations"
  will build new types from old types.

- A "product type" pair-wise joins the possible instances together to make
  instances of a new data type.

- A "sum type" unions the possible instances together into a new data type.

- To start with, we have a type Unit with exactly one inhabitant ().

- Example of a product type -- Boolean Pair:

    - This is pseudo-code for a product type.

        ```
        data BoolPair = BPair(Bool, Bool)
        ```

    - BoolPair is our type

    - BPair is a data constructor.

    - Data constructors can viewed as functions that generate an instance of
      our type (`Bool => Bool => BoolPair`, for example).

    - Note that because Bool has two possible instances (True and False),
      BoolPair has four (2*2) possible instances, which gives some intuition
      of why it's called a product type:

        - BPair(True, True)
        - BPair(True, False)
        - BPair(False, True)
        - BPair(False, False)

- Example of sum type -- Boolean:

    - This is pseudo-code for a sum type introducing a new notation `|`.

        ```
        data Bool = True(Unit) | False(Unit)
        ```

    - Bool is our type.

    - True and False are data constructors that each accept a Unit value.

    - Bool can be either True or False, but not both, so `|` is often
      pronounced "or".

    - Bool has two possible instances -- one from True's Unit and one from
      False's Unit. This gives some intuition of why it's called a sum type.

    - The Unit instance only helps with pedagogy. It's kind of like writing
      `3*1` instead of just `3`. In this regard, Bool as described above can be
      viewed as a sum of products types.

    - Practical programming languages don't force the application of a
      singleton `()` value, and you can say something more like

        ```
        data Bool = True | False
        ```

    - To use a sum type, we need a facility called "pattern matching" to
      take advantage of the different data constructors:

        ```
        someBool match {
          case True => ifTrue
          case False => ifFalse
        }
        ```

    - You should note that pattern-matching over a boolean is equivalent to an
      if-then-else expression.

    - With pattern matching, we must address all possible cases of a sum type.

    - Pattern matching generalizes an if-then-else expression to all possible
      custom sum types we can create.

    - Note that once a sum type is defined, you can't add more cases without
      breaking all of your pattern matches.

- All languages have algebraic data types. Colloquially when we say a language
  doesn't, we mean the language doesn't have a syntax for sum types. Almost
  all languages have a syntax for product types (classes, interfaces, tuples,
  etc.).

- Languages without sum types can encode the idea of them as a variant of the
  "visitor pattern."


Algebraic Data Types in Scala
-----------------------------

- Scala does not offer a direct syntax for ADTs.  Instead, we encode ADTs in a
  similar way to how we can encode classes into Javascript even without
  Ecmascript 6's "class" keyword.

- Scala offers small language features that together build to make this
  encoding:

    - The "case" keyword

        - makes a class have public members by default.

        - autogenerates the following members: equals, hashCode, apply,
          unapply, toString.

    - The "sealed" keyword makes a class or trait require all subtypes be
      declared in the same file.  This gives us the ability to get the finite
      non-extensible set of options for a sum type.


Using Types Well
----------------

- What's often called the "Curry-Howard correspondence" yields an interesting
  relationship: type signatures are to programs, as theorems are to proofs.

- Different type systems correspond to different logical theories.

- If we use Scala's type system well, we can have some of the benefits of
  logic.

- Scala's type system by itself does not correspond to a consistent/sound
  logic, but all is not lost, if we limit ourselves to the subset that does.

- When we use Scala with more discipline, we can have
    - sum types correspond to disjunction
    - product types correspond to conjunction
    - function types correspond to material implication
    - generics correspond to universal quantification
    - abstract types correspond to existential quantification.


Unfiled Comments
----------------

- (=>) is right associative
- point-free
    - (f andThen g) ≡ { x => g(f(x)) }
- definition of "higher order function"
- a fold function (catamorphism) for an ADT captures the essence of
    - pattern matching
    - recursion
- by-name evaluation policy (uses => notation)


Advanced Interest
-----------------

- List[A] is kind of like a type function (A => List[A])
- List[Int] is a type where we've applied Int to (A => List[A])
- Just as _values_ have _types_, _types_ have _kinds_
- Int has kind `*`
- List[Int] has kind `*`
- List is said to have kind `* -> *` (F[_] in Scala)
- Map is said to have kind `* -> * -> *` (F[_, _] in Scala)
- Functor is said to have kind `(* -> *) -> *` (F[G[_]] in Scala)


[SBTDOCS]: http://www.scala-sbt.org/documentation.html
[SBTEXTRAS]: https://github.com/paulp/sbt-extras
[SBTPLUGIN]: https://github.com/CognitiveScale/c12e-sbt-plugin
[WARTREMOVER]: https://github.com/wartremover/wartremover
[CHAINEDPKGS]: http://www.scala-lang.org/docu/files/package-clauses/packageclauses.html
