package su.wps.modelfactory.reflection

import org.specs2.mutable.Specification

case class Customer(name: String)

class FieldReflectorSpec extends Specification {
  "FieldReflector should" >> {
    "set value correctly" >> {
      val name = "me"
      val customer = Customer(name)
      customer.name mustEqual name

      val reflector = new FieldReflector(customer)

      val newName = "someone"
      reflector.setV("name", newName)

      customer.name mustEqual newName
    }

    "return correct value" >> {
      val name = "me"
      val customer = Customer(name)

      val reflector = new FieldReflector(customer)

      reflector.getV("name") mustEqual name
    }
  }
}
