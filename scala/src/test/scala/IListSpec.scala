package com.c12e.learn
package test


import com.c12e.learn.data.IList


object IListSpec extends Spec("IList") {
  checkAll("semigroup", Props.semigroup[IList[String]])
}
