package su.wps.modelfactory.helpers

import org.specs2.mutable.Specification
import su.wps.modelfactory.Factory

import scala.util.Success

sealed trait MyTrait extends FactoryTrait

case class MyObj1(v: String)
case class MyObj2(v: String, i: Int)

class FactoryObj1 extends FactoryObject[MyObj1] with MyTrait {
  def register() = {
    Factory.register[MyObj1]() { m =>
      m.v.mapsTo("some1")
    }
  }
}

class FactoryObj2 extends FactoryObject[MyObj2] with MyTrait {
  def register() = {
    Factory.register[MyObj2]() { m =>
      m.v.mapsTo("some2") and
      m.i.mapsTo(2)
    }
  }
}

class ScannerSpec extends Specification with SpecHelper {
  register[MyTrait]()

  "Scanner should" >> {
    "register all subclasses of trait" >> {
      val mo1t = new FactoryObj1().create()
      val mo2t = new FactoryObj2().create()

      mo1t.isInstanceOf[Success[_]] mustEqual true
      mo2t.isInstanceOf[Success[_]] mustEqual true

      val mo1 = mo1t.get
      val mo2 = mo2t.get
      mo1.v mustEqual "some1"
      mo2.v mustEqual "some2"
      mo2.i mustEqual 2
    }
  }
}
