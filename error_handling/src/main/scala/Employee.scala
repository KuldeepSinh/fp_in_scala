package fpinscala.errorhandling

case class Employee(name: String, department: String, manager: MyOption[String])

object EmployeeDirectory {
  val joni = Employee("Joni", "Sales", None)
  val joe = Employee("Joe", "Sales", Some("Joni"))
  val bob = Employee("Bob", "Purchase", Some("Alice"))

  def lookupByName(name: String): MyOption[Employee] = name match {
    case "Joni" => Some(joni)
    case "Joe" => Some(joe)
    case "Bob" => Some(bob)
    case _ => None
  }

  def lookupByNameEither(name: String): MyEither[String, Employee] = name match {
    case "Joni" => Right(joni)
    case "Joe" => Right(joe)
    case "Bob" => Right(bob)
    case _ => Left(s"No employee found by name = $name.")
  }

  def insuranceRateQuote(age: Int, tickets: Int): Int = age * tickets

  def parseInsuranceRateQuote(age: String, tickets: String): MyOption[Int] = {
    val optAge: MyOption[Int] = Try(age.toInt)
    val optTickets: MyOption[Int] = Try(tickets.toInt)
    MyOption.map2Option(optAge, optTickets) (insuranceRateQuote)
  }

  def Try[A](a: => A): MyOption[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }
}
