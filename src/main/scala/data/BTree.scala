package com.c12e.learn
package data

sealed trait BTree[A] {
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def fold[B](ifLeaf: A => B)(ifNode: (B, B, A) => B): B =
    this match {
      case Leaf(v) => ifLeaf(v)
      case Node(l, r, v) => ifNode(
        l.fold(ifLeaf)(ifNode),
        r.fold(ifLeaf)(ifNode),
        v
      )

    }
}

final case class Leaf[A](value: A) extends BTree[A]
final case class Node[A](left: BTree[A], right: BTree[A], value: A) extends BTree[A]


object TestBTree extends App {
  val testTree: BTree[Int] = Node(Leaf(1), Node(Leaf(1), Leaf(2), 5), 5)
  println(testTree)
  val sumTree: Int = testTree.fold(x => x)((x, y, v) => x + y + v)
  println(sumTree)
}