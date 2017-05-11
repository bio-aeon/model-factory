package su.wps.modelfactory.fields

import org.specs2.mutable.Specification

case class UserId(id: Int) extends AnyVal

case class User(id: UserId)

case class WithIntId(id: Int)

class FieldBuilderSpec extends Specification {
  "FieldBuilder should" >> {
    "map a property to a specified value" >> {
      val propName = "id"
      val fieldBuilder = new FieldBuilder(propName) {
        type objectType = User
        type fieldType = UserId
      }

      val id = UserId(1)
      val setter = fieldBuilder.mapsTo(id)
      setter mustNotEqual null
      setter.propName mustEqual propName
    }

    "map a property to a random value" >> {
      val propName = "id"
      val fieldBuilder = new FieldBuilder(propName) {
        type objectType = WithIntId
        type fieldType = Int
      }
      val setter = fieldBuilder.isRandom
      setter mustNotEqual null
      setter.getValue mustNotEqual null
    }
  }
}
