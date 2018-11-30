package fpinscala.lazyness

sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }
}
final case object Empty extends MyStream[Nothing]
final case class Cons[+A] (h: () => A, //non-strict definition of head.
                           t: () => MyStream[A]  //non-strict definition of tail.
                          ) extends MyStream[A]

object MyStream {
  def cons[A](h: => A, t: => MyStream[A]): MyStream[A] = {
    lazy val hd = h //lazy serves memoizing values too
    lazy val tl = t
    Cons(() => hd, () => tl)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}