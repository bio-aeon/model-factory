package su.wps.modelfactory

import objects.ObjectSetter
import scala.util.Random
import scala.language.implicitConversions

/**
  * This package object will have the implicit conversion to turn a FieldSetter to an
  * ObjectSetter with only one setter
  */
package object fields {

  implicit def fieldSetterToObjectSetter[O](setter : FieldSetter[O, Any]): ObjectSetter[O] =
    new ObjectSetter[O](List(setter))

  /**
    * This are classes that can create random values
    */
  trait Randomizer[T] extends (() => T){

    val random = new Random()

    def apply() : T
  }

  implicit object IntRandomizer extends Randomizer[Int] {

    def apply() : Int = random.nextInt()
  }

  implicit object DoubleRandomizer extends Randomizer[Double] {
    def apply() : Double = random.nextDouble()
  }

  implicit object LongRandomizer extends Randomizer[Long] {
    def apply() : Long = random.nextLong()
  }

  implicit object StringRandomizer extends Randomizer[String] {
    def apply() : String = randomAlphaString(10)(random)
  }

  private def randomString(alphabet: String)(n: Int)(implicit random: Random): String =
    Stream.continually(random.nextInt(alphabet.length)).map(alphabet).take(n).mkString

  private def randomAlphaString(n: Int)(implicit random: Random) =
    randomString("abcdefghijklmnopqrstuvwxyz")(n)

}
