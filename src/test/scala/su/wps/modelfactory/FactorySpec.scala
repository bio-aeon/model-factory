package su.wps.modelfactory

import org.specs2.mutable.Specification

case class User(id: Int, name: String)

class FactorySpec extends Specification {
  "Factory should" >> {
    "construct a user correctly" >> {
      val id = 1
      val name = "me"
      Factory.register[User]() { user =>
        user.id.mapsTo(id) and
        user.name.mapsTo(name)
      }

      val user = Factory.build[User]
      user.id mustEqual id
      user.name mustEqual name
    }
  }
}
