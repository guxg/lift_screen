package code.snippet

object Gender extends Enumeration {

  type Gender = Value

  val Male = Value(0, "Male")

  val Female = Value(1, "Female")

  val allTypes = Array(Male, Female)
}