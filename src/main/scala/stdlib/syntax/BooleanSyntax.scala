package com.c12e.learn
package stdlib
package syntax


class BooleanOps(val a: Boolean) extends AnyVal {
  def implies(b: Boolean) = !a || b
}

trait BooleanSyntax {
  implicit def toBooleanOps(a: Boolean): BooleanOps = new BooleanOps(a)
}

object BooleanSyntax extends BooleanSyntax
