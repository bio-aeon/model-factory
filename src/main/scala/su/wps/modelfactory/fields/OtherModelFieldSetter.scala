package su.wps.modelfactory.fields

import su.wps.modelfactory.Factory

import scala.reflect.ClassTag

/**
  * This is a FieldSetter that will be mapped to another model registered in Factory
  * This will be used for nested creation
  */
class OtherModelFieldSetter[O, F](propName: String, clazz: Class[_])
  extends FieldSetter[O, F](propName,clazz) {

  def getValue: F = Factory.build[F]()(ClassTag(clazz))
}
