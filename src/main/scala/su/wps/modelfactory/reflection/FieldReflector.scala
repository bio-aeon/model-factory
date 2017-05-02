package su.wps.modelfactory.reflection

import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{TypeTag, typeTag}

/**
  * This class is a helper to set and get values of a certain field property by reflection
  */
class FieldReflector[O](ref: O)(implicit tag : TypeTag[O]) {
  def getV(name: String): Any =
    getField(name).get

  def setV[T](name: String, value: T) {
    getField(name).set(value)
  }

  private def getField(name : String) = {
    val mirror = universe.runtimeMirror(ref.getClass.getClassLoader)
    val fieldTermSymb = tag.tpe.decl(universe.TermName(name)).asTerm
    val classTag = ClassTag[O]( typeTag[O].mirror.runtimeClass(typeTag[O].tpe))
    val instanceMirror = mirror.reflect(ref)(classTag)
    instanceMirror.reflectField(fieldTermSymb)
  }
}
