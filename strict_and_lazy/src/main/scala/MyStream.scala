package fpinscala.lazyness

import scala.annotation.tailrec
import MyStream._


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
    case (Empty, _) => empty
    case (_, 0) => empty
    case (Cons(h, t), _) => cons(h(), t().take(n-1))
  }


  //exercise 5.2
  def tailRecTake(n: Int): MyStream[A] =  {
    def go(s: MyStream[A], n: Int, acc: MyStream[A]): MyStream[A] = (s, n) match {
      case (Empty, _) => acc
      case (_, 0) => acc
      case (Cons(h, t), _) => go(t(), n-1, cons(h(),acc))
    }
    go(this, n, empty)
  }

  //exercise 5.2
  def drop(n: Int): MyStream[A] = (this, n) match {
    case (Empty, _) => empty
    case (_, 0) => this
    case (Cons(_, t), _) => t().drop(n-1)
  }

  //exercise 5.3
  def takeWhile(p: A => Boolean): MyStream[A] = (this, p) match {
    case (Empty, _) => empty
    case (Cons(h, t), p) => p(h()) match {
      case true => cons(h(), t().takeWhile(p))
      case false => empty
    }
  }

  //exercise 5.3
  def tailRecTakeWhile(p: A => Boolean): MyStream[A] = {
    def go(s: MyStream[A], p: A=>Boolean, acc: MyStream[A]): MyStream[A] = (s, p) match {
      case (Empty, _) => acc
      case (Cons(h, t), p) => p(h()) match {
        case true => go(t(), p, cons(h(), acc))
        case false => acc
      }
    }
    go(this, p, empty)
  }

  def exists(p: A => Boolean): Boolean = {
    def go(s: MyStream[A], p: A=> Boolean, acc: Boolean): Boolean = s match {
      case Cons(h, t) => p(h()) || go(t(), p, acc)
      case _ => acc
    }
    go(this, p, false)
  }

  def foldRight[B] (z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def foldedExists(p: A => Boolean): Boolean = foldRight(false) ((a,b) => p (a) || b)


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

  //exercise 5.8
  def constant[A] (a: A): MyStream[A] = cons(a, constant(a))
}