package su.wps.modelfactory.fields

import su.wps.modelfactory.Factory

import scala.reflect.api
import scala.reflect.runtime.universe._

/**
  * This is a FieldSetter that will be mapped to another model registered in Factory
  * This will be used for nested creation
  */
class OtherModelFieldSetter[O, F](propName: String, clazz: Class[_])
  extends FieldSetter[O, F](propName,clazz) {

  def getValue: F = {
    val mirror = runtimeMirror(clazz.getClassLoader)
    val typ = mirror.classSymbol(clazz).selfType
    val typeTag = TypeTag[F](mirror, new api.TypeCreator {
      def apply[U <: api.Universe with Singleton](m: api.Mirror[U]): U # Type =
        if (m eq mirror) typ.asInstanceOf[U # Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be " +
          s"migrated to other mirrors.")
    })

    Factory.build[F]()(typeTag)
  }
}
