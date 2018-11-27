package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait MyList[+A]
final case object Nil extends MyList[Nothing]
final case class Cons[+A] (head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] = { //variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))
  }

  //exercise 3.2
  def tail[A](as: MyList[A]): MyList[A] = {
    drop(as, 1)
  }

  //exercise 3.3
  def setHead[A](a: A, as: MyList[A]): MyList[A] = {
    Cons(a, as)
  }

  //exercise 3.4
  @tailrec
  def drop[A](as: MyList[A], n: Int): MyList[A] = (as, n)  match {
    case (as, 0) => as
    case (Nil, _) => Nil
    case (Cons(_, xs), n) => drop(xs, n-1)
  }

  //exercise 3.5 (check the curried syntax, there is no comma between 2 args of the method)
  def dropWhile[A](as: MyList[A]) (f: A => Boolean): MyList[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => f(x) match {
      case true => dropWhile(xs) (f) //notice : the curried syntax
      case false => Cons(x,xs)
    }
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  //exercise 3.6
  def init[A](as: MyList[A]): MyList[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  //foldRight
  def foldRight[A, B] (as: MyList[A], unity: B) (f: (A, B) => B): B = as match {
    case Nil => unity
    case Cons(x, xs) => f(x, foldRight(xs, unity) (f))
  }

  def sum2(ns: MyList[Int]) = foldRight(ns, 0) ((x, y) => x + y)
  def product2(ns: MyList[Double]) = foldRight(ns, 1.0) (_ * _) //(_ * _) is concise notation for ((x,y) => x*y)
  //exercise 3.9
  def length[A](as: MyList[A]): Int = foldRight(as, 0) ((_, acc) => acc + 1)

  //exercise 3.10
  @tailrec
  def foldLeft[A,B](as: MyList[A], unity: B)(f: (B, A) => B): B = as match {
    case Nil => unity
    case Cons(x, xs) => foldLeft(xs, f(unity, x)) (f)
  }

  @tailrec
  def foldLeft2[A,B](as: MyList[A])(unity: B)(f: (B, A) => B): B = as match {
    case Nil => unity
    case Cons(x, xs) => foldLeft2(xs) (f(unity, x)) (f)
  }

  //exercise 3.11
  def sum3(ns: MyList[Int]) = foldLeft(ns, 0) ((x, y) => x + y)
  def product3(ns: MyList[Double]) = foldLeft(ns, 1.0) (_ * _) //(_ * _) is concise notation for ((x,y) => x*y)
  def length2[A](as: MyList[A]): Int = foldLeft(as, 0) ((acc, _) => acc + 1)

  //exercise 3.12
  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, MyList[A]()) ((x, y) => Cons(y, x))

  //exercise 3.14
  def append2[A](as: MyList[A])(bs: MyList[A]): MyList[A] = foldLeft2(as) (bs) ((x, y) => Cons(y, x))
  def append3[A](as: MyList[A],bs: MyList[A]): MyList[A] = foldRight(as, bs) ((x, y) => Cons(x, y))

}