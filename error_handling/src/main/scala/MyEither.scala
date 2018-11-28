package fpinscala.errorhandling

sealed trait MyEither[+E, +A] {

  //exercise 4.6
  def map[B](f: A => B): MyEither[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  //exercise 4.6
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  //exercise 4.6
  def orElse[EE >: E,B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  //exercise 4.6
  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = ???
}
final case class Left[+E] (value: E) extends MyEither[E, Nothing]
final case class Right[+A] (value: A) extends MyEither[Nothing, A]