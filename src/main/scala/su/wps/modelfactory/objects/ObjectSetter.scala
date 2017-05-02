package su.wps.modelfactory.objects

import su.wps.modelfactory.fields.FieldSetter

/**
  * This class holds a List of FieldSetters.
  *
  * This class was mainly created for syntactic sugar for the user
  */
class ObjectSetter[+O](val fieldSetters : List[FieldSetter[O, Any]]) {

  def and[B >: O](setter : FieldSetter[B, Any]) = new ObjectSetter[B](setter :: fieldSetters)

  def overrideFields[B >: O](objectSetter : ObjectSetter[B]) = {
    new ObjectSetter[B](fieldSetters.filter(setter =>
      !objectSetter.fieldSetters.contains(setter)) ++ objectSetter.fieldSetters)
  }
}
