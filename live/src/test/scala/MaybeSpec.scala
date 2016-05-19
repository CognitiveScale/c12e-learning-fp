package com.c12e.learn
package test


import com.c12e.learn.data.{ Max, Maybe }
import com.c12e.learn.stdlib.instances._


object MaybeSpec extends Spec("maybe") {
  checkAll(Props.equal[Maybe[String]])
  checkAll(Props.monoid[Maybe[Max[Int]]])
  checkAll(Props.applicative[Maybe, Int, String, (Int, String)])
}
