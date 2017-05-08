package su.wps.modelfactory.reflection

import org.specs2.mutable.Specification
import su.wps.modelfactory.fields.{FieldSetter, SpecifiedFieldSetter}

case class ModelWithString(strAttr: String)

case class ModelWithInt(intAttr: Int)

case class WithMutableAttr(intAttr: Int) {
  var mutableAttr = ""
}

class ObjectReflectorSpec extends Specification {
  "ObjectReflector should" >> {
    "create an instance of the class with string attr correctly" >> {
      val strAttr = "str"
      val strAttrSetter = new SpecifiedFieldSetter[ModelWithString, String](
        "strAttr",
        strAttr,
        strAttr.getClass)
      val model = ObjectReflector.create(List(strAttrSetter))
      model.strAttr mustEqual strAttr
    }

    "create an instance of the class with int attr correctly" >> {
      val intAttr = 123
      val intAttrSetter = new SpecifiedFieldSetter[ModelWithInt, Int](
        "intAttr",
        intAttr,
        intAttr.getClass)
      val model = ObjectReflector.create(List(intAttrSetter))
      model.intAttr mustEqual intAttr
    }

    "set all fields including non-constructor" >> {
      val intAttr = 123
      val mutableAttr = "str"
      val intAttrSetter = new SpecifiedFieldSetter[WithMutableAttr, Int](
        "intAttr",
        intAttr,
        intAttr.getClass)
      val mutableAttrSetter = new SpecifiedFieldSetter[WithMutableAttr, String](
        "mutableAttr",
        mutableAttr,
        mutableAttr.getClass)
      val list : List[FieldSetter[WithMutableAttr, Any]] = List(intAttrSetter, mutableAttrSetter)
      val model = ObjectReflector.create(list)
      model.intAttr mustEqual intAttr
      model.mutableAttr mustEqual mutableAttr
    }
  }
}
