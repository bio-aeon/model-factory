package su.wps.modelfactory

import org.specs2.mutable.Specification
import su.wps.modelfactory.objects.ObjectBuilder

case class User(id: Int, name: String)

case class WithOptionalName(name: Option[String])

case class WithOptionalNames(names: Option[List[String]])

case class Company(name : String)

case class Worker(name : String, company : Company)

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

    "construct a model with an optional value" >> {
      val name = Some("me")
      Factory.register[WithOptionalName]() { withOptional =>
        withOptional.name.mapsTo(name)
      }

      val withOptional = Factory.build[WithOptionalName]
      withOptional.name mustEqual name
    }

    "construct a model with an optional list" >> {
      val names = Some(List("name1", "name2"))
      Factory.register[WithOptionalNames]() { withOptional =>
        withOptional.names.mapsTo(names)
      }

      val withOptional = Factory.build[WithOptionalNames]
      withOptional.names mustEqual names
    }

    "construct a model with None value" >> {
      Factory.register[WithOptionalName]() { withOptional =>
        withOptional.name.mapsTo(None)
      }

      val withOptional = Factory.build[WithOptionalName]
      withOptional.name mustEqual None
    }

    "construct two user models with different templates" >> {
      val name = "me"
      val id = 1
      val hisName = "he"

      Factory.register[User]() { user =>
        user.id.mapsTo(id) and
        user.name.mapsTo(name)
      }

      Factory.register[User](Some('he)) { user =>
        user.id.mapsTo(id) and
        user.name.mapsTo(hisName)
      }

      val me = Factory.build[User]
      me.id mustEqual id
      me.name mustEqual name

      val he = Factory.build[User](Some('he))
      he.name mustEqual hisName
    }

    "construct a user with overriders" >> {
      val id = 1
      val name = "me"
      Factory.register[User]() { user =>
        user.id.mapsTo(id) and
        user.name.mapsTo(name)
      }

      val newName = "someone"
      val user = Factory.build[User] { (user: ObjectBuilder[User]) =>
        user.name.mapsTo(newName).alone
      }

      user.id mustEqual id
      user.name mustEqual newName
    }

    "construct a user with overriders using alternative syntax" >> {
      val id = 1
      val name = "me"
      Factory.register[User]() { user =>
        user.id.mapsTo(id) and
        user.name.mapsTo(name)
      }

      val newName = "someone"
      val user = Factory.build[User]('name -> newName)

      user.id mustEqual id
      user.name mustEqual newName
    }

    "construct an object with related model correctly" >> {
      val companyName = "myCompany"
      val workerName = "someWorker"
      Factory.register[Company]() { company =>
        company.name.mapsTo(companyName).alone
      }

      Factory.register[Worker]() { worker =>
        worker.name.mapsTo(workerName) and
        worker.company.isAnotherFactoryModel
      }

      val worker = Factory.build[Worker]

      worker.name mustEqual workerName
      worker.company.name mustEqual companyName
    }
  }
}
