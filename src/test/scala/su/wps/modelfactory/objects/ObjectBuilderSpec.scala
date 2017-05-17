package su.wps.modelfactory.objects

import org.specs2.mutable.Specification

case class User(name: String)

class ObjectBuilderSpec extends Specification {
  "ObjectBuilder should" >> {
    "create field builder" >> {
      val builder = ObjectBuilder[User]()
      val nameBuilder = builder.name
      nameBuilder.propName mustEqual "name"
    }
  }
}
