package su.wps.modelfactory

import java.util.concurrent.ConcurrentHashMap

import su.wps.modelfactory.fields.SpecifiedFieldSetter
import su.wps.modelfactory.objects.{ObjectBuilder, ObjectSetter}
import su.wps.modelfactory.reflection.ObjectReflector

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

object Factory {
  private[this] val models = new ConcurrentHashMap[Symbol, ObjectSetter[Any]]().asScala

  def register[O](symbol : Option[Symbol] = None)
                 (model : ObjectBuilder[O] => ObjectSetter[O])
                 (implicit tag : ClassTag[O]) {
    models += ((mapKey(symbol), model(ObjectBuilder[O]())))
  }

  def build[O] (
      symbol : Option[Symbol],
      overriders : (ObjectBuilder[O] => ObjectSetter[O]))(implicit tag : ClassTag[O]) : O = {
    val creator = models.getOrElse(
      mapKey(symbol),
      throw new IllegalStateException(s"No builder register for $symbol")
    ).asInstanceOf[ObjectSetter[O]]
    ObjectReflector.create(creator.overrideFields(overriders(ObjectBuilder[O]())).fieldSetters)
  }

  def build[O] (symbol : Option[Symbol], attributes: (Symbol, Any)*)
               (implicit tag : ClassTag[O]) : O = {
    val creator = models.getOrElse(
      mapKey(symbol),
      throw new IllegalStateException(s"No builder register for $symbol")
    ).asInstanceOf[ObjectSetter[O]]
    val objectBuilder = ObjectBuilder[O]()

    val fieldSetters = attributes.map { case (attrName, attrValue) =>
      val fieldType = tag.runtimeClass.getDeclaredField(attrName.name).getType
      new SpecifiedFieldSetter(attrName.name, attrValue, fieldType)
    }.toList

    val overriders = (model : ObjectBuilder[O] ) => new ObjectSetter[O](fieldSetters)
    ObjectReflector.create(creator.overrideFields(overriders(objectBuilder)).fieldSetters)
  }

  def build[O](symbol : Option[Symbol])(implicit tag : ClassTag[O]): O = {
    build(symbol, (model : ObjectBuilder[O] ) => new ObjectSetter[O](List()))
  }

  def build[O](implicit tag : ClassTag[O]): O = build(None)

  def build[O] (
      overriders : (ObjectBuilder[O] => ObjectSetter[O]) = (model : ObjectBuilder[O] ) => new ObjectSetter[O](List()))
     (implicit tag : ClassTag[O]) : O = {
    build(None, overriders)
  }

  def build[O](attributes: (Symbol, Any)*)(implicit tag : ClassTag[O]) : O = {
    build(None, attributes: _*)
  }

  private def mapKey[O](symbol : Option[Symbol])(implicit tag : ClassTag[O]) =
    symbol.getOrElse(ObjectReflector.classSymbol[O])
}
