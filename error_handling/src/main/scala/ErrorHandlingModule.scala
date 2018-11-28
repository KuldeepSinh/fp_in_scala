import fpinscala.errorhandling.{Employee, EmployeeDirectory, MyOption, None, Some}


object ErrorHandlingModule {
  def main(args: Array[String]): Unit = {
    println("Hello, error handler!")

    //mean tests
    println("\nmean tests")
    println(mean(Seq(1.0,2.0,3.0)))
    println(mean(Seq()))

    //variance tests
    println("\nvariance tests")
    println(variance(Seq(1.0,2.0,3.0, 4.0)))
    println(variance(Seq(1.0, 2.0, 1.0, 2.0)))
    println(variance(Seq()))

    //map tests
    println("\nmap tests")
    println(EmployeeDirectory.lookupByName("Joe").map(_.department))
    println(EmployeeDirectory.lookupByName("Alice").map(_.department))

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


  }

  def mean(xs: Seq[Double]): MyOption[Double] = xs.isEmpty match {
    case true => None
    case false => Some (xs.sum / xs.length)
  }

  //exercise 4.2
  def variance(xs: Seq[Double]): MyOption[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m , 2))))
  }
}
