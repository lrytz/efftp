package scala.tools.nsc.effects.testing

import language.experimental.macros
import reflect.macros.Context

object TestMacros {
  def isSubtype[T](expr: Any): Boolean = macro isSubtypeImpl[T]

  def isSubtypeImpl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[Any]): c.Expr[Boolean] = {
    import c.universe._
    val res = Literal(Constant(expr.tree.tpe <:< c.weakTypeOf[T]))
    c.Expr[Boolean](res)
  }
}
