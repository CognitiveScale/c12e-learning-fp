package com.c12e.learn
package stdlib
package instances


import com.c12e.learn.typeclass.Equal


trait BooleanInstances {

  implicit def booleanEqual: Equal[Boolean] = Equal.fromObject[Boolean]

}


object BooleanInstances extends BooleanInstances
