import fpinscala.datastructures.{Cons, MyList, Nil}
import fpinscala.datastructures.MyList.{sum, tail, setHead, drop, dropWhile, append, init, length, sum2, product2, length2, sum3, product3, reverse, append2, append3}

object MainModule {
  def main(args: Array[String]): Unit = {
    println("Hello, World!")

    val a: MyList[Int]  = Nil
    val b: MyList[Int]  = Cons(1, a)
    val c: MyList[Int]  = Cons(2, b)
    val d: MyList[Int]  = Cons(3, c)
    println(d)
    println(MyList(1,2,3,4,5,6,7,8,9,10))

    //exercise 3.1
    val x = MyList(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //this pattern will match, resulting x = 3
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println("\nExercise 3.1 output")
    println(x)

    //tail-tests
    println("\ntail-tests")
    println(tail(MyList(1,2,3,4,5,6,7,8,9,10)))
    println(tail(Nil))
    println(tail(Cons(1, Nil)))
    println(tail(Cons(1,Cons(2,Nil))))

    //set-head tests
    println("\nsetHead tests")
    println(setHead(42, MyList(1,2,3,4,5,6,7,8,9,10)))
    println(setHead(42, a))

    //drop tests
    println("\ndrop tests")
    println(drop(MyList(1,2,3,4,5,6,7,8,9,10), 2))
    println(drop(MyList(1,2,3,4,5,6,7,8,9,10), 0))

    //dropWhile tests
    println("\ndropWhile tests")
    println(dropWhile(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x < 6)) //notice the curried syntax while calling the function
    println(dropWhile(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x > 6)) //notice the curried syntax while calling the function

    //append tests
    println("\nappend test")
    println(append(MyList(1,2,3), MyList(11,22,33)))

    //init tests
    println("\ninit tests")
    println(init(Nil))
    println(init(Cons(1, Nil)))
    println(init(Cons(2, Cons(1, Nil))))
    println(init(MyList(1,2,3)))

    //foldRight tests
    println("\nfoldRight tests")
    println(sum2(MyList(1,2,3)))
    println(product2(MyList(1,2,3,4,5)))
    println(length(MyList(1,2,3)))

    //foldLeft tests
    println("\nfoldLeft tests")
    println(sum3(MyList(1,2,3)))
    println(product3(MyList(1,2,3,4,5)))
    println(length2(MyList(1,2,3)))

    //reverse tests
    println("\nreverse tests")
    println(reverse(Nil))
    println(reverse(Cons(1, Nil)))
    println(reverse(MyList(1,2,3)))

    //append2 test
    println("\nappend2 test")
    println(append2(MyList(1,2,3))((MyList(11,22,33))))
    //append3 test
    println("\nappend3 test")
    println(append3(MyList(1,2,3), MyList(11,22,33)))
  }

}
