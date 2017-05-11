package su.wps.modelfactory.reflection

/**
  * This class is a helper to set and get values of a certain field property by reflection
  */
class FieldReflector[O](ref: O) {
  def getV(name: String): Any =
    getField(name).get(ref)

  def setV[T](name: String, value: T) {
    getField(name).set(ref, value)
  }

  private def getField(name : String) = {
    val field = ref.getClass.getDeclaredField(name)
    field.setAccessible(true)
    field
  }
}
