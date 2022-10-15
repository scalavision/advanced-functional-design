package net.degoes.afd.rules

import scala.language.implicitConversions
import zio._

// Facts[("age", Int), ("name", String), ("isAlive", Boolean)]
sealed trait Expr[-In, +Out] { self =>

  /* From Slack
      final def ++ [In1 <: In, Fields1, Fields2](that: Expr[In1, Facts[Fields2]])(implicit ev: Out <:< Facts[Fields1]): Expr[In1, Facts[Fields1 & Fields2]] =
        Expr.CombineFacts(self.widen[Facts[Fields1]], that)

   */
  final def ++[In1 <: In, Fields1, Fields2](that: Expr[In1, Facts[Fields2]])(implicit
    ev: Out <:< Facts[Fields1]
  ): Expr[In1, Facts[Fields1 & Fields2]] =
    Expr.CombineFacts(self.widen[Facts[Fields1]], that)

  final def +[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Add, tag)

  final def -[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Sub, tag)

  final def *[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Mul, tag)

  final def /[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit tag: Numeric[Out1]): Expr[In1, Out1] =
    Expr.BinaryNumericOp(self.widen, that, Expr.NumericBinOpType.Div, tag)

  final def &&[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
    Expr.And(self.widen[Boolean], that)

  final def ||[In1 <: In](that: Expr[In1, Boolean])(implicit ev: Out <:< Boolean): Expr[In1, Boolean] =
    Expr.Or(self.widen[Boolean], that)

  final def unary_!(implicit ev: Out <:< Boolean): Expr[In, Boolean] =
    Expr.Not(self.widen[Boolean])

  final def ===[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    Expr.EqualTo(self, that)

  final def !=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1]): Expr[In1, Boolean] =
    !(self === that)

  final def <[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit ev: PrimitiveType[Out1]): Expr[In1, Boolean] =
    Expr.LessThan(self, that)

  final def <=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit ev: PrimitiveType[Out1]): Expr[In1, Boolean] =
    (self < that) || (self === that)

  final def >[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit ev: PrimitiveType[Out1]): Expr[In1, Boolean] =
    !(self < that)

  final def >=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit ev: PrimitiveType[Out1]): Expr[In1, Boolean] =
    !(self < that) || (self === that)

  final def ifTrue[In1 <: In, Out2](
    ifTrue: Expr[In1, Out2]
  )(implicit
    ev: Out <:< Boolean
  ): Expr.IfTrue[In1, Out2] = Expr.IfTrue(self.widen[Boolean], ifTrue)

  final def eval(facts: In): Out = Expr.eval(facts, self)

  // Out extends Out2, every Dog is an Animal
  // This is only done at compiletime, never being used at runtime
  // This is a typesafe cast
  final def widen[Out2](implicit ev: Out <:< Out2): Expr[In, Out2] =
    self.asInstanceOf[Expr[In, Out2]]

  final def >>>[Out2](that: Expr[Out, Out2]): Expr[In, Out2] =
    Expr.Pipe(self, that)

}

object Expr {
  // Expr(true).ifTrue(42).otherwise(43)
  // Syntax class
  final case class IfTrue[In, Out](condition: Expr[In, Boolean], ifTrue: Expr[In, Out]) {
    def otherwise(ifFalse: Expr[In, Out]): Expr[In, Out] = Expr.IfThenElse(condition, ifTrue, ifFalse)

  }
  // does not need an input
  final case class Fact[In, K <: Singleton & String, V](
    factDef: FactDefinition.KeyValue[K, V],
    value: Expr[In, V]
  ) extends Expr[In, Facts[(K, V)]]

  final case class CombineFacts[In, V1, V2](
    left: Expr[In, Facts[V1]],
    right: Expr[In, Facts[V2]]
  ) extends Expr[In, Facts[V1 & V2]]

  final case class Constant[Out](value: Out, tag: EngineType[Out])            extends Expr[Any, Out]
  final case class And[In](left: Expr[In, Boolean], right: Expr[In, Boolean]) extends Expr[In, Boolean]
  final case class Or[In](left: Expr[In, Boolean], right: Expr[In, Boolean])  extends Expr[In, Boolean]
  final case class Not[In](value: Expr[In, Boolean])                          extends Expr[In, Boolean]
  final case class EqualTo[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])   extends Expr[In, Boolean]
  final case class LessThan[In, Out](lhs: Expr[In, Out], rhs: Expr[In, Out])  extends Expr[In, Boolean]
  // Used to create an Input, it is an identity. It lets Expr refer to its input
  final case class Input[K <: Singleton with String, V, In](
    factDef: FactDefinition.KeyValue[K, V]
  ) extends Expr[(K, V), V]

  final case class BinaryNumericOp[In, Out](
    lhs: Expr[In, Out],
    rhs: Expr[In, Out],
    op: NumericBinOpType,
    tag: Numeric[Out]
  ) extends Expr[In, Out]

  final case class Pipe[In, Out1, Out2](
    left: Expr[In, Out1],
    right: Expr[Out1, Out2]
  ) extends Expr[In, Out1 with Out2]

  final case class IfThenElse[In, Out](
    condition: Expr[In, Boolean],
    ifTrue: Expr[In, Out],
    ifFalse: Expr[In, Out]
  ) extends Expr[In, Out]

  implicit def apply[Out](out: Out)(implicit tag: PrimitiveType[Out]): Expr[Any, Out] =
    Constant(out, EngineType.Primitive(tag))

  implicit def apply[Out](out: Facts[Out]): Expr[Any, Facts[Out]] =
    Constant(out, EngineType.fromFacts[Out](out))

  // def evalWithType[In, Out](in: Facts[In], expr: Expr[In,Out]): (EngineType[Out], Out) = ???

  def eval[In, Out](in: In, expr: Expr[In, Out]): Out =
    expr match {
      case Fact(factDef, value) =>
        implicit val tag = factDef.tag
        Facts.empty.add(factDef, value)

      case CombineFacts(left, right) =>
        val leftFacts  = eval(in, left)
        val rightFacts = eval(in, right)
        leftFacts ++ rightFacts

      case Constant(value, tag) => value

      case And(left, right) =>
        val lhs = eval(in, left)
        val rhs = eval(in, right)
        lhs && rhs

      case Or(left, right) =>
        val lhs = eval(in, left)
        val rhs = eval(in, right)
        lhs || rhs

      case Not(value) =>
        !eval(in, value)

      case EqualTo(lhs, rhs) =>
        val l = eval(in, lhs)
        val r = eval(in, rhs)
        l == r // FIXME: not correct for all types

      case LessThan(lhs, rhs) =>
        val l = eval(in, lhs)
        val r = eval(in, rhs)

        ???

      case Input(factDef) =>
        val fieldValue = Unsafe.unsafe { implicit u =>
          //in.unsafe.get(factDef)
          ???
        }
        fieldValue.asInstanceOf[Out]

      case Pipe(left, right) => ???

      case BinaryNumericOp(lhs, rhs, op, tag0) =>
        val tag   = tag0.asInstanceOf[Numeric[Out]]
        val left  = eval(in, lhs)
        val right = eval(in, rhs)
        tag(op)(left, right)

      case IfThenElse(condition, ifTrue, ifFalse) =>
        val bool = eval(in, condition)
        if (bool) eval(in, ifTrue)
        else eval(in, ifFalse)
    }

  // Introducer for Expr, we also need an eliminator
  //def input[A](implicit tag: PrimitiveType[A]): Expr[A, A] = Input(tag)
  def input[K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V]): Expr[(K, V), V] =
    Input(factDef)

  def fact[In, K <: Singleton with String, V](
    factDef: FactDefinition.KeyValue[K, V],
    value: Expr[In, V]
  ): Expr[In, Facts[(K, V)]] =
    Fact(factDef, value)

  def ifThenElse[In, Out](condition: Expr[In, Boolean])(
    ifTrue: Expr[In, Out],
    ifFalse: Expr[In, Out]
  ): Expr[In, Out] =
    IfThenElse(condition, ifTrue, ifFalse)

  sealed trait NumericBinOpType
  object NumericBinOpType {
    case object Add    extends NumericBinOpType
    case object Sub    extends NumericBinOpType
    case object Mul    extends NumericBinOpType
    case object Div    extends NumericBinOpType
    case object Modulo extends NumericBinOpType
  }

  def evalWithType[In, Out](in: Facts[In], expr: Expr[In, Out]): (EngineType[Out], Out) = ???

}
