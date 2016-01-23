package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = sys.error("todo")

  def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")

  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = sys.error("todo")

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
      case _ => Nil }

def append[A](a1: List[A], a2: List[A]): List[A] =
  foldRight(a1,a2)((x,xs) => Cons(x,xs))

// can sort of figure this out just from knowing
// the type of append
//foldRight accumulates arguments differently so it's faster to use
def flatten[A](l: List[List[A]]): List[A] = 
    List.foldRight(l, Nil:List[A])(List.append)

def addOneToAll(l: List[Int]): List[Int] = map(l)(_+1)
//foldRight then z appears at the end
def showDoubles(doubles: List[Double]) = foldLeft(doubles, "")(_.toString + ", " + _.toString).tail.tail

def filter[A](l: List[A])( f: (A => Boolean)): List[A] = l match {
    case Cons(x, xs) if f(x) =>  Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
    case _ => Nil 
}

filter(List())(_ % 2 == 0)
val flatMap = (map _) andThen 
def flatMap[A,B](l: List[A])(f: (A => List[B])): List[B] = flatten(map(l)(f))


def flatMapFilter[A](l: List[A])( f: (A => Boolean)): List[A] = 
    flatMap(l)(x => if (f(x)) List(x) else Nil)


def addEach(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(x, xs), Cons(y,ys)) => Cons(x + y, addEach(xs, ys))
    case _ => Nil
}

  
def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Cons(x, xs), Cons(y,ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => Nil
}
def exists[A](l: List[A])(f: A=>Boolean): Boolean = l match {
    case cons(x, xs) if f(x) => true
    case cons(x, xs) => exists(xs)(f)
    case _ => false 
}

val contains = (xs, e) => exists(xs)(_ == e)

def contains[A](l: List[A], e: A): Boolean = exists(l)(_ == e)
def head[A](l: List[A]): A = l match { 
   case Cons(x, _) => x
   case _ => throw new Exception() }

def tail[A](l: List[A]) = l match { 
   case Cons(_, xs) => xs
   case _ => throw new Exception() }

def tails[A](l: List[A]): List[List[A]] = l match {
    case Cons(x, xs) => Cons(xs, tails(xs))
    case _ => Nil }

def length[A](l: List[A]): Int = List.foldRight(l, 0)((_,acc) => acc + 1)
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = 
    contains(map(tails(sup))((xs) => length(zipWith(xs, sub)(_ == _))), length(sub))



def hasSubsequence[A](sup: Seq[A], sub: Seq[A]): Boolean =
    val tails = sup.tails.filter(_.length >= sub.length)
    tails.map(xs => (xs, sub).zipped.forall(_ == _)).contains(true)
