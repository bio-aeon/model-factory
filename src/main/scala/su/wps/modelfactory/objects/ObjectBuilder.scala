package su.wps.modelfactory.objects

import su.wps.modelfactory.fields.FieldBuilder

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
  * The object builder is the interface that is going to be used to define the model
  * to be used in Factory.
  *
  * An example of usage would be
  *
  * val user = ObjectBuilder[User]
  * user.name.mapsTo("me")
  *
  * This class uses Macro (Scala 2.10 feature) to make this test object factory type safe.
  *
  * What does this mean? when in the previos example you do "user.name", if first checks
  * that name is a field of user. If name isn't a field of user, then it won't compile.
  * Then, it sets the type of that field in the FieldBuilder so that if name is of type String
  * you cannot do user.name.mapsTo(2). This will not compile
  */
class ObjectBuilder[O] extends Dynamic {

  def selectDynamic(propName: String): FieldBuilder = macro ObjectBuilder.createFieldBuilder[O]

}

object ObjectBuilder {

  def apply[O]() = new ObjectBuilder[O]

  def createFieldBuilder[O: c.WeakTypeTag](c: Context)(propName: c.Expr[String]) = {
    import c.universe._

    def doesntCompile(reason: String) = c.abort(c.enclosingPosition, reason)

    val propertyName = propName.tree match {
      case (Literal(Constant(propertyName: String))) => propertyName
      case _ => doesntCompile("The property isn't a string literal")
    }

    val objectType = c.weakTypeOf[O]

    val fieldMember = objectType.member(TermName(propertyName)) orElse {
      doesntCompile(s"The property $propertyName isn't a field of $objectType")
    }

    val fieldMemberType = fieldMember.typeSignatureIn(objectType) match {
      case NullaryMethodType(tpe) => tpe
      case _ => doesntCompile(s"$propertyName isn't a field, it must be another thing")
    }

    c.Expr(Block(List(ClassDef(Modifiers(Flag.FINAL), TypeName("$anon"), List(),
      Template(
        List(
          Select(
            Select(
              Select(
                Select(
                  Ident(TermName("su")),
                  TermName("wps")
                ),
                TermName("modelfactory")
              ),
              TermName("fields")
            ),
            TypeName("FieldBuilder")
          )
        ),
        noSelfType, List(DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List(Literal(Constant(propertyName))))), Literal(Constant(())))), TypeDef(Modifiers(), TypeName("objectType"), List(), Ident(TypeName(objectType.typeSymbol.name.toString))), TypeDef(Modifiers(), TypeName("fieldType"), List(), TypeTree(fieldMemberType)))))), Apply(Select(New(Ident(TypeName("$anon"))), termNames.CONSTRUCTOR), List())))


  }

}
