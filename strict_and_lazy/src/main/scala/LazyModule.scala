import fpinscala.lazyness.MyStream

object LazyModule {
  def main(args: Array[String]): Unit = {
    println("Hello Lazy Scala!")

    //lazy if
    println("\nlazyIf test")
    lazyIf(40 > 55,
      () => println("Its a lazyIf true."),
      () => println("Its a lazyIf false."))

    //lazy if scala style
    lazyIf2(40 > 55,
      () => println("Its a lazyIf2 true."),
      () => println("Its a lazyIf2 false."))

    //code showing multiple evaluation of a value
    println("\nStrict evaluation of a clause")
    val x = mayBeTwice(true, { println("hi"); 1+41 })
    println(x)

    //code showing lazy evaluation of a value, with value being cached.
    println("\nLazy evaluation of a value and value being cached.")
    val y = mayBeTwiceWithLazyValue(true, { println("hi"); 1+41 })
    println(y)

    //MyStream Tests
    println("\nheadOption tests")
    println(MyStream(1,2,3).headOption)
    println(MyStream().headOption)

    //toList tests
    println("\ntoList tests")
    println(MyStream().toList)
    println(MyStream(1,2,3).toList)
    println(MyStream(1,2,3).tailRecToList)

    //take tests
    println("\ntake tests")
    println(MyStream(1,2,3).take(0).toList)
    println(MyStream(1,2,3).take(1).toList)
    println(MyStream(1,2,3).take(2).toList)
    println(MyStream(1,2,3).take(3).toList)

    //drop tests
    println("\ndrop tests")
    println(MyStream(1,2,3).drop(0).toList)
    println(MyStream(1,2,3).drop(1).toList)
    println(MyStream(1,2,3).drop(2).toList)
    println(MyStream(1,2,3).drop(3).toList)


//    //takeWhile tests
//    println("\ntakeWhile tests")
//    println(MyStream.empty.takeWhile(n => n >= 1).toList)
//    println(MyStream(1,2,3).takeWhile(n => n >= 1).toList)
//    println(MyStream(1,2,3).takeWhile(n => n >= 1 && n <= 2).toList)
//    println(MyStream(1,2,3).takeWhile(n => n >= 1 && n <= 3).toList)
//    println(MyStream(1,2,3).takeWhile(n => n >= 2).toList)

  }

  def lazyIf[A](condition: Boolean, onTrue: () => A, onFalse: () => A): A = { //here, () => A is a thunk
    if (condition) onTrue() else onFalse() //here, we are explicitly forcing thunk ot be evaluated by calling onTrue() and OnFalse()
  }

  //Notice : change in the scala syntax of onTrue and OnFalse
  def lazyIf2[A](condition: Boolean, onTrue: => A, onFalse: => A): A = { //scala syntax for defining thunks
    if (condition) onTrue else onFalse //scala syntax for forcing thunks to evaluate
  }


  // With either of above syntax (lazyIf and lazyIf2),
  // an argument that’s passed unevaluated to a function will be
  // evaluated once for each place it’s referenced in the body of the function
  def mayBeTwice(condition: Boolean, i: => Int) =
    if(condition) {i + i} //we are calling i for multiple times (twice) and adding the output of i
    else {0}

  def mayBeTwiceWithLazyValue(condition: Boolean, i: => Int): Int = {
    lazy val j = i //here val j is declared as lazy. So, it will be calculated when needed. And its value will be cached.
    if(condition) {j + j}
    else 0
  }

}
