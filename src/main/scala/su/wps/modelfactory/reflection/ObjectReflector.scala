package su.wps.modelfactory.reflection

import su.wps.modelfactory.fields.FieldSetter

import scala.reflect.runtime.universe._

/**
  * This is the object that does the magic.
  *
  * Given a list of FieldSetters and the Type of the object to create, this object first finds
  * the minimum constructor to which we have all of the proeprties needed in our FieldSetters.
  * After this, using this constructor, it instantiates the object. Then, using the remaining
  * FieldSetters, this sets all of the fields value the user has asked.
  */
object ObjectReflector {

  def create[T, Any](fieldSetters : List[FieldSetter[T, Any]])(implicit tag : TypeTag[T]) : T = {

    val constructorList = typeOf[T].decl(termNames.CONSTRUCTOR).asTerm.alternatives.collect {
      case m : MethodSymbol => m.paramLists.map(_.map(x => x.asInstanceOf[TermSymbol]))
    }.flatten

    val minConstructor = constructorList.minBy(_.size)

    val namesToUse = minConstructor.map(x => x.name.toString)

    val clazzToUse = clazz[T]
    val clazzName = clazzToUse.getSimpleName

    val params = namesToUse.map(name =>
      fieldSetters.find(setter => setter.propName == name).getOrElse(
        throw new IllegalStateException(s"The constructor for $clazzName needs a param with name $name and there's no property with that value")
      ))

    val classesToUse = params.map(param => param.getValueClass)

    val reflectedConstructor = {
      val mirror = runtimeMirror(getClass.getClassLoader)
      val classSymbol = mirror.classSymbol(clazzToUse)
      val classMirror = mirror.reflectClass(classSymbol)
      val ctor = classSymbol.toType.decl(termNames.CONSTRUCTOR).asMethod
      classMirror.reflectConstructor(ctor)
    }

    val instance = reflectedConstructor(params.map(_.getValue.asInstanceOf[Object]): _*).asInstanceOf[T]

    val fieldsRemaining = fieldSetters.dropWhile(elem => params.contains(elem))

    fieldsRemaining.foreach(_.setValue(instance))

    instance
  }

  def clazz[T](implicit tag : TypeTag[T]): Class[_] =
    tag.mirror.runtimeClass(tag.tpe).asInstanceOf[Class[T]]

  def classSymbol[T](implicit tag : TypeTag[T]) = Symbol(clazz[T].getName)
}
