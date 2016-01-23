package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // size inc. leaves and branches
  def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
  }
  
  def maximum(t: Tree[Int]): Int =  {
      def go(t: Tree[Int], y: Int): Int = t match {
          case Leaf(x) => x max y
          case Branch(l, r) => go(l, y) max go(r, y)
      }
      go(t, Int.MinValue)
  }
  
  def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}
