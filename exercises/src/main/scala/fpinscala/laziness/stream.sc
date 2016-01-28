import scala.collection.immutable.Stream
import scala.{Option => _, Some => _, Either => _, _}
import scalaz.{\/-, -\/, \/}
trait Stream[+A] {
  import Stream._
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList(): List[A] = this.foldRight(List():List[A])(_ +: _)

  def take(n: Int): Stream[A] = (this, n) match {
    case (_, 0) => Empty
    //Can't use the Cons() constructor directly cuz type stuff.
    case (Cons(h, t), n) => cons(h(), t().take(n - 1))
    case (Empty, _) => throw new RuntimeException("Tried to take too much.")
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, xs) => xs
    case (n, Cons(h, t)) => t().drop(n-1)
    case (_, Empty) => sys.error("tried to drop too much")
  }

  def tail: Stream[A] =  this.drop(1)

  // this works because foldRight goes backwards,
  // evaluating the end of the list first!
  // and it's lazy because `xs` is only evaluated (and the recursion only continues)
  // if p(x) is true. Once it's false, it stops, evaluating x (or h), and returning empty!
  def takeWhile(p: A => Boolean): Stream[A] =
    this.foldRight(empty:Stream[A])(
      (x, xs) => if (p(x)) cons(x, xs) else empty)
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  def headEither: String \/ A =
    this.foldRight(-\/("not there"):String \/ A)(
      (h, e) => if (h == empty) e else \/-(h))

  def headOption: Option[A] =
    this.foldRight(None:Option[A])((h, _) => if (h == Empty) None else Some(h))

  def map[B](f: (A => B)): Stream[B] =
    this.foldRight(empty[B])((x, z) => cons(f(x), z))

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])(
      (x, z) => if (p(x)) cons(x, z) else z)

  //def append[B <: A](ys: Stream[B]): Stream[B]
  def append[B >: A](ys: Stream[B]): Stream[B] =
    ys.foldRight(this:Stream[B])(
      (h, zs) => cons(h, zs))

  def flatMap[B](f: (A => Stream[B])): Stream[B] =
    this.map(f).foldRight(empty[B])( (xs, zs) => zs.append(xs))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  def repeat[A](a: A): Stream[A] = {
    lazy val as:Stream[A] = cons(a, as)
    as
  }

//  val fibs = {
//    def go(f0: Int, f1: Int): Stream[Int] = {
//      cons(f0, go(f1, f0 + f1))
//      go(0, 1)
//    }
//  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]) = {
    def go(res: Option[(A, S)]): Stream[A] = res match {
      case None => empty
      case Some((v, xs)) => cons(v, go(f(xs)))
    }
    go(f(z))
  }
  def map[A,B](xs: Stream[A])(f: A => B): Stream[B] =
    unfold(xs)({
      case Cons(x, xs) => Some((f(x()), xs()))
      case empty => None
    })

  def take[A](xs: Stream[A], n: Int): Stream[A] =
    unfold((n, xs))({
      case (0, _) => None
      case (n, Cons(x, xs)) => Some((x(), (n-1, xs())))
      case (n, empty) => sys.error("taking too much")
    })

  def takeWhile[A](xs: Stream[A])(p: A => Boolean): Stream[A] =
    unfold(xs)( {
      case Cons(x, xs) if (p(x())) => Some((x(), xs()))
      case _ => None
    })

  def zipWith[A,B,C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((as, bs))({
      case (Cons(x, xs), Cons(y, ys)) => Some((f(x(), y()), (xs(), ys())))
      case _ => None
    })

  def constant(n: Int) = unfold(n)(x => Some((x, x)))

  def from(n: Int) = unfold(n)(x => Some(x, x+1))

  def fibs() = unfold( (0, 1))({case (x,y) => Some(y, (y, x+y))})

  def zipAll[A,B,C](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((as, bs))({
      case (Cons(x, xs), Cons(y, ys)) => Some((Some(x()), Some(y())), (xs(), ys()))
      case  (Cons(x, xs), _) => Some( (Some(x()), None), (xs(), empty))
      case  (_, Cons(y, ys)) => Some((None, Some(y())), (empty, ys()))
      case _ => None
    })

  def startsWith[A](sup: Stream[A], sub: Stream[A]): Boolean =
    !(takeWhile(zipAll(sup, sub))(x => x._2 != None).exists(x => x._1 != x._2))
//NOTE: need to match on upper-case Case, otherwise taken as an identifier!
  // e.g. Empty/empty in this func
  def tails[A](as: Stream[A]): Stream[Stream[A]] =
    Stream.cons(as, unfold(as)({
      case Empty => None
      case xs => Some(xs.tail, xs.tail)
    }))

  def hasSubSequence[A](sup: Stream[A], sub:Stream[A]): Boolean =
    tails(sup).exists(xs => startsWith(xs, sub))
}

// create private constructors to enforce Non-empty type!
//


//Stream.repeat(3).take(3).toList() == List(3, 3, 3)
//Stream.constant(3).take(3).toList() == List(3, 3, 3)
//Stream.from(3).take(3).toList //== List(3, 4, 5)
////Stream.fibs(4).toList()
////Stream.from(3).take(3).toList //== List(3, 4, 5)
//Stream.fibs().take(5). toList
//Stream.map(Stream.constant(1))(_ + 3).take(3).toList
//Stream.map(Stream.from(3))(_ + 10).takeip(3).toList
Stream.take(Stream.constant(3), 3).toList
Stream.takeWhile(Stream.from(1))(_ < 3).toList
Stream.zipWith(Stream.constant(1), Stream.from(3).take(3))(_ + _).toList
Stream.zipAll(Stream.constant(3).take(4), Stream.constant(9).take(2)).toList
Stream.startsWith(Stream(1, 2, 3), Stream(1, 2,3)) //true cases
Stream.startsWith(Stream(1, 2, 3), Stream(1, 2))
Stream.startsWith(Stream(1, 2, 3), Stream(1))
Stream.startsWith(Stream(1, 2, 3), Stream(1, 4)) // false casesk
Stream.startsWith(Stream(1, 2, 3), Stream(4))
Stream.startsWith(Stream(2, 3), Stream(2, 3, 4))
Stream.tails(Stream(10, 11, 12, 13)).toList map(_.toList)

Stream.hasSubSequence(Stream(1, 2, 3, 4, 5, 6), Stream(3, 4))