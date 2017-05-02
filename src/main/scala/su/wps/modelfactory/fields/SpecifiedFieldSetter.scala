package su.wps.modelfactory.fields

/**
  * This is the most basic FieldSetter. It maps a cetain field to a certain value.
  */
class SpecifiedFieldSetter[O, F](propName: String, value: F, clazz: Class[_])
  extends FieldSetter[O, F](propName, clazz) {

  def getValue: F = value
}
