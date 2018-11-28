import fpinscala.datastructures.{Branch, Cons, Leaf, MyList, MyTree, Nil}
import fpinscala.datastructures.MyList._
import fpinscala.datastructures.MyTree._

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

    //concatenate test
    println("\nconcatenate test")
    println(concatenate(MyList(MyList(1,2,3), MyList(11,22,33))))

    //map tests
    println("\nmap tests")
    println(addOneToEach(MyList(1,2,3)))
    println(toStringEach(MyList(1.0,2.0,3.3)))


    //filter test
    println("\nfilter tests")
    println(MyList.filter(MyList(1,2,3))(x => x < 2))
    println(MyList.filter(MyList(1,2,3, 9, 2, 3, 7, 5, 0, 1, 22, 4))(x => x > 4))
    println("\nfilter2 tests")
    println(MyList.filter2(MyList(1,2,3))(x => x < 2))
    println(MyList.filter2(MyList(1,2,3, 9, 2, 3, 7, 5, 0, 1, 22, 4))(x => x > 4))

    //flatMap test
    println("\nflatMpa test")
    println(MyList.flatMap(MyList(1,2,3))(i => MyList(i, i, i)))

    //zipWith test
    println("\nzipWith test")
    println(addMyLists(MyList(1,2,3), MyList(11,22,33)))

    //take tests
    println("\ntake tests")
    println(take(Nil, 1))
    println(take(MyList(1,2,3), 0))
    println(take(MyList(1,2,3), 1))
    println(take(MyList(1,2,3), 2))
    println(take(MyList(1,2,3), 3))
    println(take(MyList(1,2,3), 4))

    //takeWhile tests
    println("\ntakeWhile tests")
    println(takeWhile(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x < 6)) //notice the curried syntax while calling the function
    println(takeWhile(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x > 6)) //notice the curried syntax while calling the function

    //forall tests
    println("\nforall tests")
    println(forall(MyList(1,2,3,4,5)) (x => x < 6))
    println(forall(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x < 6)) //notice the curried syntax while calling the function
    println(forall(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x > 6)) //notice the curried syntax while calling the function


    //forall tests
    println("\nexists tests")
    println(exists(MyList(1,2,3,4,5)) (x => x == 6))
    println(exists(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x < 6)) //notice the curried syntax while calling the function
    println(exists(MyList(1,2,3,4,5,6,7,8,9,10)) (x => x > 6)) //notice the curried syntax while calling the function

    //startsWith tests
    println("\nstartsWith tests")
    println(startsWith(MyList(1,2,3), MyList(11,2,33)))
    println(startsWith(MyList(1,2,3), MyList(1,2,3)))
    println(startsWith(MyList(1,2,3), MyList(1,2)))
    println(startsWith(MyList(1,2,3), MyList(1)))


    //subSequence tests
    println("\nsubSequence tests")
    println(subSequence(MyList(1,2,3), MyList(11,2,33)))
    println(subSequence(MyList(1,2,3), MyList(1,2,3)))
    println(subSequence(MyList(1,2,3), MyList(1,2)))
    println(subSequence(MyList(1,2,3), MyList(1)))
    println(subSequence(MyList(1,2,3,4), MyList(2,3,4)))
    println(subSequence(MyList(1,2,3,4), MyList(2,3)))
    println(subSequence(MyList(1,2,3,4), MyList(1,3)))
    println(subSequence(MyList(1,2,3,4), MyList(1,4)))
    println(subSequence(MyList(1,2,3,4), MyList(3,4)))
    println(subSequence(MyList(1,2,3,4), MyList(4,3)))
    println(subSequence(MyList(1,2,3,4), MyList(2)))
    println(subSequence(MyList(1,2,3,4), MyList(4)))


    //Tree Exercises
    //size tests
    println("\nsize tests")
    println(size(Leaf(1)))
    println(size(Branch(Leaf(1), Leaf(2))))
    println(size2(Leaf(1)))
    println(size2(Branch(Leaf(1), Leaf(2))))

    //maximum tests
    println("\nmaximum tests")
    println(maximum(Leaf(1)))
    println(maximum(Branch(Leaf(1), Leaf(2))))
    println(maximum2(Leaf(1)))
    println(maximum2(Branch(Leaf(1), Leaf(2))))

    //depth tests
    println("\ndepth tests")
    println(depth(Leaf(1)))
    println(depth(Branch(Leaf(1), Leaf(2))))
    println(depth2(Leaf(1)))
    println(depth2(Branch(Leaf(1), Leaf(2))))

    //map tests
    println("\nmap tests")
    println(fpinscala.datastructures.MyTree.map(Leaf(1))(a => a * a))
    println(fpinscala.datastructures.MyTree.map(Branch(Leaf(1), Leaf(2)))(a => a * a))
    println(map2(Leaf(1))(a => a * a))
    println(map2(Branch(Leaf(1), Leaf(2)))(a => a * a))

    //one more map test
    def l = Leaf(1)
    def b1 = Branch(Leaf(3), Leaf(4))
    def b2 = Branch(b1, l)
    println(map2(b2)(a => a * a * a))

  }

}
