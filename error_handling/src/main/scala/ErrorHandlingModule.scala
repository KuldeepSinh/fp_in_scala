import fpinscala.errorhandling.{EmployeeDirectory, Left, MyEither, MyOption, None, Right, Some}


object ErrorHandlingModule {
  def main(args: Array[String]): Unit = {
    println("Hello, error handler!")

    //mean tests
    println("\nmean Option tests")
    println(meanO(Seq(1.0,2.0,3.0)))
    println(meanO(Seq()))

    //mean tests
    println("\nmean Either tests")
    println(meanE(Seq(1.0,2.0,3.0)))
    println(meanE(Seq()))

    //variance tests
    println("\nvariance tests")
    println(variance(Seq(1.0,2.0,3.0, 4.0)))
    println(variance(Seq(1.0, 2.0, 1.0, 2.0)))
    println(variance(Seq()))

    //map Option tests
    println("\nmap Option tests")
    println(EmployeeDirectory.lookupByName("Joe").map(_.department))
    println(EmployeeDirectory.lookupByName("Alice").map(_.department))

    //map Either tests
    println("\nmap Either tests")
    println(EmployeeDirectory.lookupByNameEither("Joe").map(_.department))
    println(EmployeeDirectory.lookupByNameEither("Alice").map(_.department))

    //filter tests
    println("\nfilter tests")
    println(EmployeeDirectory.lookupByName("Joe").map(_.department).filter(_ != "Sales"))
    println(EmployeeDirectory.lookupByName("Joe").map(_.department).filter(_ != "IT"))
    println(EmployeeDirectory.lookupByName("Alice").map(_.department))

    //getOrElse tests
    println("\ngetOrElse tests")
    println(EmployeeDirectory.lookupByName("Joe").map(_.department).getOrElse("No department"))
    println(EmployeeDirectory.lookupByName("Alice").map(_.department).getOrElse("No department"))

    //flatMap tests
    println("\nflatMap tests")
    println(EmployeeDirectory.lookupByName("Joe").flatMap(_.manager))
    println(EmployeeDirectory.lookupByName("Joni").flatMap(_.manager))
    println(EmployeeDirectory.lookupByName("Alice").flatMap(_.manager))

    //getOrElse tests
    println("\nflatMap tests")
    println(EmployeeDirectory.lookupByName("Joe").flatMap(_.manager).getOrElse("Does not report to any manager."))
    println(EmployeeDirectory.lookupByName("Joni").flatMap(_.manager).getOrElse("Does not report to any manager."))
    println(EmployeeDirectory.lookupByName("Alice").flatMap(_.manager).getOrElse("Does not report to any manager."))

    //map2 Option test
    println("\nmap2 Option Tests")
    println(EmployeeDirectory.parseInsuranceRateQuote("14", "3"))
    println(EmployeeDirectory.parseInsuranceRateQuote("14", "hello"))
  }

  def meanO(xs: Seq[Double]): MyOption[Double] = xs.isEmpty match {
    case true => None
    case false => Some (xs.sum / xs.length)
  }

  def meanE(xs: Seq[Double]): MyEither[String, Double] = xs.isEmpty match {
    case true => Left("Empty List!")
    case false => Right(xs.sum / xs.length)
  }

  //exercise 4.2
  def variance(xs: Seq[Double]): MyOption[Double] = {
    meanO(xs).flatMap(m => meanO(xs.map(x => math.pow(x - m , 2))))
  }

  //for understanding : Following lines are equivalent (expanded one after the preceding one for understanding)
  //def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  //def lift[A,B](f: A => B): Option[A] => Option[B] = (_: Option[A]).map(f)
  //def lift[A,B](f: A => B): Option[A] => Option[B] = (o: Option[A]) => o.map(f)

}
