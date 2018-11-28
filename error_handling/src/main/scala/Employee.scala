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
}
