import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int =
    if(n < 0) -n
    else n

  private def formatResult(problem: String, x: Int, f: Int => Int): String = {
    val msg = "The %s value of %d is %d"
    msg.format(problem, x, f(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute", -42, abs))
    println(formatResult("factorial", 12, factorial))
    println(formatResult("fibonacci", 42, fibonacci))
    println(findFirst(Array(1,2,3), (i:Int) => i >= 3))
    println(findFirst(Array("Hello","World","!"), (str: String) => str.equals("World")))
    println(isSorted(Array(1,2,3,5,7), (i:Int, j: Int) => i <= j))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n * acc)
    }
    go(n, 1)
  }

  //exercise 2.1
  def fibonacci(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, current: Int): Int = {
      if (n == 0) prev
      else go(n - 1, current, prev+current)
    }
    go(n, 0, 1)
  }

  def findFirst[A] (as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      n match {
        case n if (n >= as.length) => -1
        case n if (p(as(n))) => n
        case _ => loop(n+1)
      }
    }
    loop(0)
  }

  //exercise 2.2
  def isSorted[A] (as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      n match {
        case n if (n >= as.length - 1) => true
        case n if (!ordered(as(n), as(n + 1))) => false
        case _ => go(n + 1)
      }
    }
    go(0)
  }

  def partial1[A, B, C] (a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  //exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  //exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  //exercise 2.5
  def compose[A, B, C]()(f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}


