package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def toList: List[A] = {
    this.foldRight[List[A]](Nil)((a, la) => a :: la)
  }

  def toListRecursive: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case Empty => acc
    }

    loop(this, List.empty[A]).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Stream.empty[A]
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A])((a, s) => if (p(a)) cons(a, s) else empty)


  def forAll(p: A => Boolean): Boolean = this.foldRight[Boolean](true)(p(_) && _)

  def headOption: Option[A] = this.foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures


  def map[B](f: A => B): Stream[B] = this.foldRight[Stream[B]](empty)((a, sb) => cons(f(a), sb))

  def filter(p: A => Boolean): Stream[A] = this.foldRight[Stream[A]](empty)((a, sa) => if (p(a)) cons(a, sa) else sa)

  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight[Stream[B]](s)((a, sa) => cons(a, sa))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight[Stream[B]](empty)((a, sb) => f(a) append sb)

  def flatMap2[B](f: A => Stream[B]): Stream[B] = this.foldRight[Stream[B]](empty)((a, sb) => f(a).foldRight[Stream[B]](sb)((aa, sbb) => cons(aa, sbb)))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(a, s) => Some((f(a()), s()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((0, this)) {
    case (acc, s) if acc < n => s match {
      case Cons(h, t) => Some(h(), (acc + 1, t()))
      case Empty => None
    }
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(a, s) if p(a()) => Some((a(), s()))
    case _ => None
  }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile {
    !_._2.isEmpty
  }.forAll {
    case (o1, o2) => o1 == o2
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this) { s =>
    s match {
      case Cons(h, t) => Some((s, t()))
      case Empty => None
    }
  } append Stream(empty)
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def loop(n: Int, nn: Int): Stream[Int] = {
      cons(n + nn, loop(nn, nn+n))
    }

    loop(0, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((aa, ss)) => cons(aa, unfold(ss)(f))
    case None => Stream.empty[A]
  }

  val onesViaUnfold: Stream[Int] = unfold(1)(s => Some((1,1)))
  val onesViaUnfold2: Stream[Int] = unfold(329393)(s => Some((1, 883283))) // it is the same

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some((a,a)))
  def constantViaUnfold2[A](a: A): Stream[A] = unfold(41552)(s => Some((a, 123535))) // it is the same

  def fibsViaUnfold: Stream[Int] = unfold((0,1))( (fs: (Int, Int)) => Some((fs._1, (fs._2, fs._1 + fs._2))))
  def fibsViaUnfold2: Stream[Int] = unfold((0,1)){ case (f0, f1) => Some((f0, (f1, f0+f1))) } // simplified with anon func.

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)( s => Some(s, s+1))
}