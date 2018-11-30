package fpinscala.lazyness

import scala.annotation.tailrec


sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  //exercise 5.1
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  //exercise 5.1
  def tailRecToList: List[A] = {
    @tailrec
    def go(s: MyStream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List()).reverse
  }

  //exercise 5.2
  def take(n: Int): MyStream[A] = (this, n) match {
    case (Empty, _) => MyStream.empty
    case (Cons(h, _), 0) => MyStream.cons(h(), MyStream.empty)
    case (Cons(h, t), n) => MyStream.cons(h(), t().take(n-1))
  }

  //exercise 5.2
  def drop(n: Int): MyStream[A] = (this, n) match {
    case (Empty, _) => MyStream.empty
    case (Cons(_, t), 0) => t()
    case (Cons(_, t), n) => t().drop(n-1)
  }

  //exercise 5.3
  def takeWhile(p: A => Boolean): MyStream[A] = ???

}


final case object Empty extends MyStream[Nothing]
final case class Cons[+A] (h: () => A, //non-strict definition of head.
                           t: () => MyStream[A]  //non-strict definition of tail.
                          ) extends MyStream[A]

object MyStream {
  //smart constructor
  def cons[A](h: => A, t: => MyStream[A]): MyStream[A] = {
    lazy val hd = h //lazy serves memoizing values too
    lazy val tl = t
    Cons(() => hd, () => tl)
  }

  //smart constructor
  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}