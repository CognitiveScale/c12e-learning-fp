package com.c12e.learn
package data

sealed trait BinaryTree[A] {

  def fold[B](ifNone: B, ifLeaf: A => B, ifNode: (B, B) => B): B = {
    this match {
      case None()     => ifNone
      case Leaf(v)    => ifLeaf(v)
      case Node(l, r) => ifNode(l.fold(ifNone, ifLeaf, ifNode), 
                                r.fold(ifNone, ifLeaf, ifNode))
    }
  }
}

final case class None[A]() extends BinaryTree[A]
final case class Leaf[A](v: A) extends BinaryTree[A]
final case class Node[A](l: BinaryTree[A], r: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
  def none[A](): BinaryTree[A] = None()

  def leaf[A](v: A): BinaryTree[A] = Leaf(v)

  def node[A](l: BinaryTree[A], r: BinaryTree[A]): BinaryTree[A] = Node(l, r)
}


object RunBinaryTree extends App {
  import BinaryTree.{none, leaf, node}

  val t: BinaryTree[Int] = node(node(none(), leaf(2)), node(leaf(3), leaf(4)))
  println(t)
  val s: Int = t.fold(0, identity, (x:Int, y:Int) => x + y)
  println("sum: " + s.toString())

  val l: List[Int] = t.fold(List[Int](), 
                            x => List[Int](x), 
                            (x:List[Int], y:List[Int]) => x ++ y)
  print ("list: " + l.toString())
}
