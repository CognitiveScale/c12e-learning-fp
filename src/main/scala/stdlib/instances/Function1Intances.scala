package com.c12e.learn
package stdlib
package instances


import com.c12e.learn.typeclass.Applicative


trait Function1Instances {

  implicit def function1Applicative[A]: Applicative[A => ?] =
    new Applicative[A => ?]{
      def map[I, J](fi: A => I)(f: I => J): A => J =
        f compose fi
      
      def pure[I](i: I): A => I =
        a => i

      def ap[I, J](fi: A => I)(fij: A => I => J): A => J =
        a => fij(a)(fi(a))
    }

}


object Function1Instances extends Function1Instances
