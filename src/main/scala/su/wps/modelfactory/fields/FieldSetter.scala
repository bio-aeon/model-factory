package su.wps.modelfactory.fields

import su.wps.modelfactory.objects.ObjectSetter
import su.wps.modelfactory.reflection.FieldReflector

import scala.reflect.runtime.universe.TypeTag

/**
  * A FieldSetter is used to set a certain field to an instance of an object.
  *
  * The result of configuring ObjectBuilders with FieldBuilders is a list of FieldSetters
  * that can create the object given what the user stated in the model.
  */
abstract class FieldSetter[+O, +F](val propName: String, clazz: Class[_]) {

  def setValue[B >: O](obj : B)(implicit tag : TypeTag[B]) {
    new FieldReflector(obj).setV(propName, getValue)
  }

  def getValue : F

  def alone = new ObjectSetter[O](List(this))

  def getValueClass : Class[_] = clazz
}
