package su.wps.modelfactory.helpers

import org.specs2.mutable.Specification
import su.wps.modelfactory.Factory

case class TestCls1(v: String)
case class TestCls2(v: String)
case class TestCls3(v: String)

object TestCls2Obj extends FactoryObject[TestCls2] {
  def register() = {
    Factory.register[TestCls2]() { t =>
      t.v.mapsTo("some2")
    }
  }
}

class HelperSpec extends Specification {
  "Helper should" >> {
    "make it possible to define new FactoryObjects" >> {
      object TestObj extends FactoryObject[TestCls1] {
        def register() {
          Factory.register[TestCls1]() { t =>
            t.v.mapsTo("some1")
          }
        }
      }

      TestObj.register()
      Factory.build[TestCls1].v mustEqual "some1"
    }

    "make it possible to register and retrieve FactoryObjects" >> {
      TestCls2Obj.register()
      val obj = TestCls2Obj()
      obj mustNotEqual null
      obj.v mustEqual "some2"
    }

    "make it possible to register multiple objects for one class using symbols" >> {
      object TestCls3Obj extends FactoryObject[TestCls3] {
        def register() {
          Factory.register[TestCls3]() { t =>
            t.v.mapsTo("some3")
          }

          Factory.register[TestCls3](Some('withSym)) { t =>
            t.v.mapsTo("some4")
          }
        }
      }

      TestCls3Obj.register()
      val noName = TestCls3Obj()
      val hasSymb = TestCls3Obj(Some('withSym))
      noName mustNotEqual null
      noName.v mustEqual "some3"
      hasSymb mustNotEqual null
      hasSymb.v mustEqual "some4"
    }
  }
}
