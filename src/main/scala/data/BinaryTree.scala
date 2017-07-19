package com.c12e.learn
package data

sealed trait BinaryTree[A] {

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def fold[B](ifTerminal: B, ifLeaf: A => B, ifNode: (B, B) => B): B = {
    this match {
      case Terminal()     => ifTerminal
      case Leaf(v)    => ifLeaf(v)
      case Node(l, r) => ifNode(l.fold(ifTerminal, ifLeaf, ifNode), 
                                r.fold(ifTerminal, ifLeaf, ifNode))
    }
  }
}

final case class Terminal[A]() extends BinaryTree[A]
final case class Leaf[A](v: A) extends BinaryTree[A]
final case class Node[A](l: BinaryTree[A], r: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
  def terminal[A](): BinaryTree[A] = Terminal()

  def leaf[A](v: A): BinaryTree[A] = Leaf(v)

  def node[A](l: BinaryTree[A], r: BinaryTree[A]): BinaryTree[A] = Node(l, r)
}


object RunBinaryTree extends App {
  import BinaryTree.{terminal, leaf, node}

  val t: BinaryTree[Int] = node(node(terminal(), leaf(2)), node(leaf(3), leaf(4)))
  println(t)
  val s: Int = t.fold(0, identity, (x:Int, y:Int) => x + y)
  println("sum: " + s.toString())

  val l: List[Int] = t.fold(List[Int](), 
                            x => List[Int](x), 
                            (x:List[Int], y:List[Int]) => x ++ y)
  print ("list: " + l.toString())
}
