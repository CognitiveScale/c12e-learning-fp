package com.c12e.learn


package object syntax {
  object all extends stdlib.AllStdlibSyntax with syntax.AllTypeclassSyntax
  object typeclass {
    object all extends syntax.AllTypeclassSyntax
    object applicative extends com.c12e.learn.typeclass.Applicative.Syntax
    object equal extends com.c12e.learn.typeclass.Equal.Syntax
    object functor extends com.c12e.learn.typeclass.Functor.Syntax
    object monoid extends com.c12e.learn.typeclass.Monoid.Syntax
    object semigroup extends com.c12e.learn.typeclass.Semigroup.Syntax
  }
}
