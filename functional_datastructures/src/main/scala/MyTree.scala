package fpinscala.datastructures

sealed trait MyTree[+A]
final case class Leaf[A](value: A) extends MyTree[A]
final case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  //exercise 3.25
  def size[A](t: MyTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) =>  1 + size(left) + size(right)
  }

  //exercise 3.26
  def maximum(t: MyTree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  //exercise 3.27
  def depth[A](t: MyTree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  //exercise 3.28
  def map[A, B](t: MyTree[A]) (f: A => B): MyTree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map (left) (f), map (right) (f))
  }

  //exercise 3.29
  def fold[A, B](t: MyTree[A])(f: A => B) (g: (B,B) => B ): B = t match {
    case Leaf(a) => f(a)
    case Branch(left,right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size2[A](t: MyTree[A]): Int = fold(t)(_ => 1)(1 +_ +_)
  def maximum2(t: MyTree[Int]): Int = fold (t) (a => a) (_ max _)
  def depth2[A](t: MyTree[A]): Int = fold (t) (a => 0) ((d1, d2) => 1 + (d1 max d2))
  def map2[A, B](t: MyTree[A]) (f: A => B): MyTree[B] = fold (t) (a => Leaf(f(a)): MyTree[B]) (Branch (_, _))
}
