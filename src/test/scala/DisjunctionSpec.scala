package com.c12e.learn
package test


import com.c12e.learn.data.\/
import com.c12e.learn.stdlib.instances._


object DisjunctionSpec extends Spec("disjunction") {
  checkAll(Props.equal[String \/ Int])
  checkAll(Props.applicative[String \/ ?, Int, String, (Int, String)])
}
