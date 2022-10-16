package net.degoes.afd.rules

final case class Condition[-In](expr: Expr[In, Boolean]) { self =>

  def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
    Condition(self.expr && that.expr)

  def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
    Condition(self.expr || that.expr)

  def unary_! : Condition[In] = Condition(!self.expr)

  def eval(facts: In): Boolean = expr.eval(facts)
}

object Condition {
  val always: Condition[Any]                      = constant(true)
  val never: Condition[Any]                       = constant(false)
  def constant[In](value: Boolean): Condition[In] = Condition(Expr(value))

}
