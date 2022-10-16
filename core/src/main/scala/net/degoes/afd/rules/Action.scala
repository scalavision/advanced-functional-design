package net.degoes.afd.rules

trait Action[-In, +Out] { self =>

  final def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
    Action.Concat(self, that)
  //Action(in => self.update(in) ++ that.update(in))

  final def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
    Action.Pipe(self, that)

  final def update[In1 <: In](in: In1) = ???
  //Action.FromExpr(in => update(in).flatMap(that.update))

  def eval(facts: In): List[Out] =
    self match {
      case Action.Concat(left, right) =>
        left.eval(facts) ++ right.eval(facts)

      case Action.Pipe(left, right) =>
        left.eval(facts).flatMap(right.eval(_))

      case Action.FromExpr(expr) =>
        List(expr.eval(facts))
    }

}
object Action {
  final case class Concat[In, Out](
    left: Action[In, Out],
    right: Action[In, Out]
  ) extends Action[In, Out]

  final case class Pipe[In, Out1, Out2](
    left: Action[In, Out1],
    right: Action[Out1, Out2]
  ) extends Action[In, Out2]

  final case class Constant[Out](
    value: Out,
    pt: PrimitiveType[Out]
  ) extends Action[Any, Out]

  final case class FromExpr[In, Out](expr: Expr[In, Out]) extends Action[In, Out]

  def serializable[Out](c: Constant[Out]): String =
    c.pt match {
      case PrimitiveType.ByteType    => c.value.toString
      case PrimitiveType.ShortType   => c.value.toString
      case PrimitiveType.CharType    => c.value.toString
      case PrimitiveType.IntType     => c.value.toString
      case PrimitiveType.DoubleType  => c.value.toString
      case PrimitiveType.StringType  => c.value.toString
      case PrimitiveType.FloatType   => c.value.toString
      case PrimitiveType.LongType    => c.value.toString
      case PrimitiveType.BooleanType => c.value.toString
      case PrimitiveType.InstantType => c.value.toString
    }

  def fromExpr[In, Out](expr: Expr[In, Out]): Action[In, Out] = FromExpr(expr)

}
