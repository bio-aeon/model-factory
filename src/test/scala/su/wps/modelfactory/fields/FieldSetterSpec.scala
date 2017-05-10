package su.wps.modelfactory.fields

import org.specs2.mutable.Specification

case class Customer(id: Int, name: String)

class FieldSetterSpec extends Specification {
  "FieldSetter should" >> {
    "set the value correctly" >> {
      val customer = Customer(1, "me")
      val propName = "id"
      val value = 5
      val setter = new SpecifiedFieldSetter[Customer, Int](propName, value, value.getClass)
      setter.setValue(customer)
      customer.id mustEqual value
    }
  }
}
