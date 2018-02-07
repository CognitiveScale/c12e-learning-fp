package com.c12e.learn
package syntax
package stdlib


class BooleanOps(val a: Boolean) extends AnyVal {
  def implies(b: Boolean): Boolean = !a || b
}


trait BooleanSyntax {
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
  implicit def toBooleanOps(a: Boolean): BooleanOps = new BooleanOps(a)
}
