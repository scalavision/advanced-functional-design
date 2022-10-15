package net.degoes.afd.rules

final case class Condition[-In](expr: Expr[In, Boolean]) { self =>

  def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
    Condition(self.expr && that.expr)

  def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
    Condition(self.expr || that.expr)

  def unary_! : Condition[In] = Condition(!self.expr)

  def eval(facts: In): Boolean = expr.eval(facts)
}
// In => Boolean
//trait Condition[-In] { self =>

// We can't use a function, it's not serializable
// def contramap[In2](f: In2 => In): Condition[In2] = ???
// def contramap[In2](getter: Getter[In2, In]): Condition[In2] = ???

object Condition {
  val always: Condition[Any]                      = constant(true)
  val never: Condition[Any]                       = constant(false)
  def constant[In](value: Boolean): Condition[In] = Condition(Expr(value))
  /*
    def isEqualTo[In: PrimitiveType](rhs: In): Condition[In] =
      Condition(Expr.input[In] === Expr(rhs))

    def isLessThan[In](rhs: In)(implicit ordering: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] < Expr(rhs))

    def isGreaterThan[In](in: In)(implicit ordering: PrimitiveType[In]): Condition[In] =
      Condition(Expr.input[In] > Expr(in))
   */

}
