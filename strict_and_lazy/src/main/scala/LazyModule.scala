object LazyModule {
  def main(args: Array[String]): Unit = {
    println("Hello Lazy Scala!")

    //lazy if
    println("\nlazyIf test")
    lazyIf(40 > 55,
      () => println("Its a lazyIf true."),
      () => println("Its a lazyIf false."))

    lazyIf2(40 > 55,
      () => println("Its a lazyIf2 true."),
      () => println("Its a lazyIf2 false."))

  }

  def lazyIf[A](condition: Boolean, onTrue: () => A, onFalse: () => A): A = {
    if (condition) onTrue() else onFalse()
  }

  //Notice : change in the scala syntax of onTrue and OnFalse
  def lazyIf2[A](condition: Boolean, onTrue: => A, onFalse: => A): A = {
    if (condition) onTrue else onFalse
  }

}
