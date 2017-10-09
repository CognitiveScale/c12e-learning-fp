package com.c12e.learn
package test


import com.c12e.learn.data.First


object FirstSpec extends Spec("First") {
  checkAll("semigroup", Props.semigroup[First[Boolean]])
}
