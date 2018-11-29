package fpinscala.errorhandling

sealed trait MyOption[+A] {
  //exercise 4.1
  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  //exercise 4.1
  //notice : use of (: => B) : it indicates that argument of type B wont be evaluated until needed by the function.
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  //exercise 4.1
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  //exercise 4.1
  //notice : use of (>:) : it indicates that B must be equal to A or of a supertype of A
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case None => ob
    case Some(_) => this
  }

  //exercise 4.1
  def filter(f: A => Boolean): MyOption[A] = this match {
    case None => None
    case Some(a) => f(a) match {
      case true => Some(a)
      case false => None
    }
  }

  //exercise 4.4
  def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] = ???

  //exercise 4.5
  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = ???


}

object MyOption{
  //exercise 4.3
  def map2Option[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }



}

final case object None extends MyOption[Nothing]
final case class Some[+A] (value: A) extends MyOption[A]


