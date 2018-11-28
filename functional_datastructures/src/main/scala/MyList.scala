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
  @tailrec
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

  //exercise 3.13

  //exercise 3.14
  def append2[A](as: MyList[A])(bs: MyList[A]): MyList[A] = foldLeft2(as) (bs) ((x, y) => Cons(y, x))
  def append3[A](as: MyList[A],bs: MyList[A]): MyList[A] = foldRight(as, bs) ((x, y) => Cons(x, y))

  //exercise 3.15
  def concatenate[A] (as: MyList[MyList[A]]) = foldLeft2 (as)(Nil:MyList[A])(append)

  //exercise 3.16
  def addOne (i: Int) = i + 1
  def addOneToEach(as: MyList[Int]) = map(as) (addOne)

  //exercise 3.17
  def toStringEach(as: MyList[Double]) = map (as) (a => a.toString)


  //exercise 3.18
  def map[A,B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  //exercise 3.19
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => f(x) match {
      case true => Cons(x, filter(xs) (f))
      case false => filter(xs) (f)
    }
  }

  //exercise 3.20
  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = concatenate(map (as) (f))

  //exercise 3.21
  def filter2[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)(a => f(a) match {
      case true => MyList(a)
      case false => Nil
    })

  //exercise 3.22
  def addMyLists(as: MyList[Int], bs: MyList[Int]): MyList[Int] = zipWith(as, bs)((a,b) => a+b)

  //exercise 3.23
  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }

  def take[A](as: MyList[A], n: Int): MyList[A] = (as, n) match {
    case (Nil, _) => Nil
    case (_, 0) => Nil
    case (Cons(x, xs), n) => Cons(x, take(xs, n-1))
  }

  def takeWhile[A](as: MyList[A]) (f: A=>Boolean):MyList[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => f(x) match {
      case true => Cons(x, takeWhile(xs) (f))
      case false => Nil
    }
  }

  @tailrec
  def forall[A](as: MyList[A])(f: A => Boolean): Boolean = as match {
    case Nil => true
    case Cons(x, xs) => f(x) match {
      case true => forall(xs)(f)
      case false => false
    }
  }

  @tailrec
  def exists[A](as: MyList[A])(f: A => Boolean): Boolean = as match {
    case Nil => false
    case Cons(x, xs) => f(x) match {
      case true => true
      case false => exists(xs) (f)
    }
  }

  @tailrec
  def startsWith[A](as: MyList[A], bs: MyList[A]): Boolean = (as, bs) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(a, as), Cons(b, bs)) => (a == b) match {
      case true => startsWith(as, bs)
      case false => false
    }
  }

  //exercise 3.24
  @tailrec
  def subSequence[A](as: MyList[A], bs: MyList[A]): Boolean  = (as, bs) match {
    case (Nil, _) => false
    case (as, bs) => startsWith(as, bs) match {
      case true => true
      case false => subSequence(tail(as), bs)
    }
  }
}