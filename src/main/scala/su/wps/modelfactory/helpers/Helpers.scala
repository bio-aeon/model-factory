package su.wps.modelfactory.helpers

import su.wps.modelfactory.Factory

import scala.reflect.runtime.universe.TypeTag
import scala.util.Try

/**
  * By defining an object as a subtype of this abstract class, using Factory
  * becomes much easier. This class provides convenient access to the register method
  * defined on the Factory object and requires a register method to be implemented.

  * Example:
  *  case class Milk(fatPerc: Int)
  *
  *  object MilkO extends FactoryObject[Milk] {
  *   def register() = {
  *     Factory.register[Milk]() { m =>
  *       m.fatPerc.mapsTo(2)
  *     }
  *   }
  * }
  *
  * An object for testing can now be created simply be calling MilkO().
  *
  */
abstract class FactoryObject[T](implicit tag: TypeTag[T]) {

  def create(symbol: Option[Symbol] = None) = Try(Factory.build[T](symbol))

  def apply(symbol: Option[Symbol] = None) = Factory.build[T](symbol)

}

/**
  * @see SpecHelper
  */
trait FactoryTrait {
  def register(): Unit
}

/**
Automatically register all subclasses of a sealed trait.
  This greatly simplifies using Factory with multiple objects.

  Simply create a **sealed trait** that extends FactoryTrait and have all your
  test objects inherit from FactoryObject as well as this new trait.
  Then include this trait in your test specification and call register()
  and everything else will be taken care of. No need to register each individual
  test object.

  Example:
    sealed trait MyTestObjects extends FactoryTrait
    class MyTestObject1 extends FactoryObject[MyObject1] with MyTestObjects {
      def register() = {
        Factory.register[MyObject1]() { o =>
          o.id.mapsTo("ID1") and
          o.name.mapsTo("Name1")
        }
      }
    }
    class MyTestObject2 extends FactoryObject[MyObject2] with MyTestObjects {
      def register() = {
        Factory.register[MyObject2]() { o =>
          o.id.mapsTo("ID2") and
          o.value.mapsTo("Value2")
        }
      }
    }
    class ScannerSpec extends FunSpec with ShouldMatchers with SpecHelper {
      register[MyTestObjects]()
      // Your tests here
    }
  */
trait SpecHelper {
  def register[T: TypeTag]() {
    def sym2factory = (x: reflect.runtime.universe.Symbol) => Class.forName(x.fullName).newInstance.asInstanceOf[FactoryTrait]
    Scanner.sealedDescendants[T] map sym2factory foreach {_.register()}
  }
}
