/**
 * Executable encodings are free of boilerplate and fast and flexible. However,
 * they tend to be less powerful and principled than declarative encodings,
 * which can be used with infinitely many different interpreters, each doing
 * something different (and useful!) with the model.
 *
 * In this section, you will refactor the rule engine you created to use the
 * declarative encoding. In the process, you will discover _expressions_, which
 * are recipes to describe the production of values from other values.
 *
 * To push this model to its logical conclusion, it will be necessary to
 * eliminate all Scala functions, which raises questions about how well-typed
 * you want to make the model.
 */
package net.degoes.afd.ruleengine

import scala.language.implicitConversions
import scala.annotation.implicitNotFound

/**
 * Develop a fully declarative encoding of a rule engine. You are NOT allowed to
 * use any Scala functions in your model. Rather, your model must be purely
 * ADT-based. Attempt to make your model as type-safe as possible, but sacrifice
 * type-safety if necessary in order to avoid the embedding of Scala functions
 * in your model.
 *
 * You must develop an executor for the model which, given input and the rule
 * set, produces actions as output.
 */
object declarative {

  // Dynamic Record
  final class DR[Fields] private (
    private val map: Map[(String, PrimitiveType[_]), Any]
  ) {

    def ++(that: DR[Fields]): DR[Fields] =
      new DR(map ++ that.map)

    def add[A](name: String, value: A)(implicit
      tag: PrimitiveType[A]
    ): DR[Fields with (name.type, A)] =
      new DR(map.updated(name -> tag, value))

    def get[A](name: String)(implicit
      tag: PrimitiveType[A],
      ev: Fields <:< (name.type, A)
    ): A =
      map(name -> tag).asInstanceOf[A] //.map(_.asInstanceOf[A])

  }

  object DR {
    def empty: DR[Nothing] = new DR(Map.empty)
    import PrimitiveType._
    // DR.empty.add("name", "John Doe")
    DR.empty.add("isMale", true)(PrimitiveType.BooleanType) //.get[Int]("age")(PrimitiveType.IntType)
    //empty.add(PrimitiveType.Int, 1)

  }

  object PrimitiveType {
    implicit case object BooleanType extends PrimitiveType[Boolean]
    implicit case object ByteType    extends PrimitiveType[Byte]
    implicit case object ShortType   extends PrimitiveType[Short]
    implicit case object CharType    extends PrimitiveType[Char]
    implicit case object IntType     extends PrimitiveType[Int]
    implicit case object LongType    extends PrimitiveType[Long]
    implicit case object FloatType   extends PrimitiveType[Float]
    implicit case object DoubleType  extends PrimitiveType[Double]
    implicit case object StringType  extends PrimitiveType[String]
    implicit case object InstantType extends PrimitiveType[java.time.Instant]

    def orderingOf[A](tag: PrimitiveType[A]): scala.math.Ordering[A] =
      tag match {
        case BooleanType => scala.math.Ordering[Boolean]
        case ByteType    => scala.math.Ordering[Byte]
        case ShortType   => scala.math.Ordering[Short]
        case CharType    => scala.math.Ordering[Char]
        case IntType     => scala.math.Ordering[Int]
        case LongType    => scala.math.Ordering[Long]
        case FloatType   => scala.math.Ordering[Float]
        case DoubleType  => scala.math.Ordering[Double]
        case StringType  => scala.math.Ordering[String]
        case InstantType => scala.math.Ordering[Long].on[java.time.Instant](_.toEpochMilli())
      }
  }

  sealed trait Numeric[A]
  object Numeric {
    implicit case object ByteNumeric  extends Numeric[Byte]
    implicit case object CharNumeric  extends Numeric[Char]
    implicit case object ShortNumeric extends Numeric[Short]
    implicit case object IntNumeric   extends Numeric[Int]
    implicit case object FloatNumeric extends Numeric[Float]
    implicit case object LongNumeric  extends Numeric[Long]
  }

  import zio._

  sealed trait Json
  object Json {
    final case class Obj(fields: Map[String, Json]) extends Json
    final case class Arr(fields: Vector[Json])      extends Json
    final case class Str(value: String)             extends Json
    final case class Num(value: Double)             extends Json
    final case class Bool(value: Boolean)           extends Json
    case object Null                                extends Json
  }
  sealed trait FactDefinition[KeyValue] { self =>
    type Key <: Singleton with String
    type Value

    def name: Key

    def tag: PrimitiveType[Value]

    // provides a way for Expr to read a fact
    def get: Expr[(Key, Value), Value] = Expr.input(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]])

    def set[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] =
      Expr.fact(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]], value)

    // TODO: call set above
    def := = ???

    override final def toString(): String = s"FactDefinition($name, $tag)"
  }
  object FactDefinition {
    // Type refinements
    type KeyValue[K <: Singleton with String, V] = FactDefinition[(K, V)] { type Key = K; type Value = V }

    // When this returns a KeyValue, you will get very good type inference on the fields below
    def apply[N <: Singleton with String, T](name0: N)(implicit tag0: PrimitiveType[T]): KeyValue[N, T] =
      new FactDefinition[(N, T)] {
        type Key   = N
        type Value = T
        def name: N               = name0
        def tag: PrimitiveType[T] = tag0
      }

    def boolean[N <: Singleton with String](name0: N): KeyValue[N, Boolean] = FactDefinition[N, Boolean](name0)

    def byte[N <: Singleton with String](name0: N): KeyValue[N, Byte] = FactDefinition[N, Byte](name0)

    def double[N <: Singleton with String](name0: N): KeyValue[N, Double] = FactDefinition[N, Double](name0)

    def int[N <: Singleton with String](name0: N): KeyValue[N, Int] = FactDefinition[N, Int](name0)

    def instant[N <: Singleton with String](name0: N): KeyValue[N, java.time.Instant] =
      FactDefinition[N, java.time.Instant](name0)

    def string[N <: Singleton with String](name0: N): KeyValue[N, String] = FactDefinition[N, String](name0)
  }

  /**
   * Contains a collection of facts, whose structure is described by a phantom
   * type parameter.
   */
  sealed abstract case class Facts[+Types] private (private val data: Map[FactDefinition[_], Any]) {
    /*
    def ++[Types2](that: Facts[Types2]): Facts[Types & Types2] =
      Facts(data ++ that.data.asInstanceOf[Map[FactDefinition[_], Any]])
     */

    def get[Key <: Singleton with String, Value: PrimitiveType](pd: FactDefinition[(Key, Value)])(implicit
      subset: Types <:< (Key, Value)
    ): Value =
      data(pd).asInstanceOf[Value]

    /**
     * Returns a new facts collection with the specified fact added.
     */
    def add[Key <: Singleton with String, Value: PrimitiveType](
      pd: FactDefinition[(Key, Value)],
      value: Value
    ): Facts[Types & (Key, Value)] =
      new Facts[Types & (Key, Value)](data + (pd -> value)) {}

    private def add[Key <: Singleton with String, Value: PrimitiveType](
      name: Key,
      value: Value
    ): Facts[Types & (Key, Value)] =
      new Facts[Types & (Key, Value)](data + (FactDefinition[Key, Value](name) -> value)) {}

    object unsafe {
      def get(pd: FactDefinition[_])(implicit unsafe: Unsafe): Option[Any] = data.get(pd)
    }
  }
  object Facts {

    /**
     * An empty facts collection.
     */
    val empty: Facts[Any] = new Facts[Any](Map.empty) {}

    def apply[Key <: Singleton with String, Value: PrimitiveType](key: Key, value: Value): Facts[(Key, Value)] =
      empty.add(key, value)

    def apply[
      Key1 <: Singleton with String,
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType
    ](
      tuple1: (Key1, Value1),
      tuple2: (Key2, Value2)
    ): Facts[(Key1, Value1) & (Key2, Value2)] =
      empty.add[Key1, Value1](tuple1._1, tuple1._2).add[Key2, Value2](tuple2._1, tuple2._2)

    def apply[
      Key1 <: Singleton with String,
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType,
      Key3 <: Singleton with String,
      Value3: PrimitiveType
    ](
      tuple1: (Key1, Value1),
      tuple2: (Key2, Value2),
      tuple3: (Key3, Value3)
    ): Facts[(Key1, Value1) & (Key2, Value2) & (Key3, Value3)] =
      empty
        .add[Key1, Value1](tuple1._1, tuple1._2)
        .add[Key2, Value2](tuple2._1, tuple2._2)
        .add[Key3, Value3](tuple3._1, tuple3._2)

    def apply[
      Key1 <: Singleton with String,
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType,
      Key3 <: Singleton with String,
      Value3: PrimitiveType,
      Key4 <: Singleton with String,
      Value4: PrimitiveType
    ](
      tuple1: (Key1, Value1),
      tuple2: (Key2, Value2),
      tuple3: (Key3, Value3),
      tuple4: (Key4, Value4)
    ): Facts[(Key1, Value1) & (Key2, Value2) & (Key3, Value3) & (Key4, Value4)] =
      empty
        .add[Key1, Value1](tuple1._1, tuple1._2)
        .add[Key2, Value2](tuple2._1, tuple2._2)
        .add[Key3, Value3](tuple3._1, tuple3._2)
        .add[Key4, Value4](tuple4._1, tuple4._2)

    def apply[
      Key1 <: Singleton with String,
      Value1: PrimitiveType,
      Key2 <: Singleton with String,
      Value2: PrimitiveType,
      Key3 <: Singleton with String,
      Value3: PrimitiveType,
      Key4 <: Singleton with String,
      Value4: PrimitiveType,
      Key5 <: Singleton with String,
      Value5: PrimitiveType
    ](
      tuple1: (Key1, Value1),
      tuple2: (Key2, Value2),
      tuple3: (Key3, Value3),
      tuple4: (Key4, Value4),
      tuple5: (Key5, Value5)
    ): Facts[(Key1, Value1) & (Key2, Value2) & (Key3, Value3) & (Key4, Value4) & (Key5, Value5)] =
      empty
        .add[Key1, Value1](tuple1._1, tuple1._2)
        .add[Key2, Value2](tuple2._1, tuple2._2)
        .add[Key3, Value3](tuple3._1, tuple3._2)
        .add[Key4, Value4](tuple4._1, tuple4._2)
        .add[Key5, Value5](tuple5._1, tuple5._2)
  }

  final case class RuleEngine[-In, +Out](update: Facts[In] => Option[List[Out]]) { self =>

    def contramap[In2](f: Facts[In2] => Facts[In]): RuleEngine[In2, Out] =
      RuleEngine((in2: Facts[In2]) => self.update(f(in2)))

    def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
      RuleEngine((in: Facts[In1]) => self.update(in) orElse that.update(in))

    def updateWith[Out1 >: Out](in: Facts[In])(defaultOut: Out1, combine: (Out1, Out1) => Out1): Out1 =
      self.update(in) match {
        case None => defaultOut

        case Some(outs) =>
          outs.reduceOption(combine).getOrElse(defaultOut)
      }

    def fromFunction[In, Out](f: Facts[In] => Out): RuleEngine[In, Out] =
      RuleEngine((in: Facts[In]) => Some(List(f(in))))

  }
  object RuleEngine {

    def apply[In, Out](f: Facts[In] => Option[List[Out]]): RuleEngine[In, Out] =
      new RuleEngine(f)

    val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

    def collect[In, Out](pf: PartialFunction[Facts[In], Out]): RuleEngine[In, Out] =
      RuleEngine(in => pf.lift(in).map(List(_)))

    def constant[Out](out: Out): RuleEngine[Any, Out] = fromFunction(_ => out)

    def fromFunction[In, Out](f: Facts[In] => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

    def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] =
      RuleEngine(??? /*ruleSet.update*/ )

  }

  final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) { self =>
    def +[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules :+ that)

    def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] =
      RuleSet(self.rules ++ that.rules)

    def addRule[In1 <: In, Out1 >: Out](that: Rule[In1, Out1]): RuleSet[In1, Out1] = self + that

  }

  object RuleSet {
    def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] = RuleSet(
      rule1 +: rules.toVector
    )

    val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
  }

  final case class Rule[-In, +Out](condition: Condition[In], action: Action[In, Out])

  @implicitNotFound("The type ${A} is not supported as a fact type and cannot be used as a parameter to this method")
  sealed trait PrimitiveType[A] { self =>
    def ordering: scala.math.Ordering[A] = PrimitiveType.orderingOf(self)
  }

  // recipe that will produce a value
  // expression 2 * 3 will produce a value
  // Description of an expression
  // 2 * (3 + 123) / "foo".length
  // Expression is pure
  // Declarative model of expression, basis for doing
  // computation and manipulation
  // how to construct values
  // Not a full programming language, a locked down subset
  // Language of calculators is an example
  // Description of how to compute a value of type Out
  // () => Out
  // In and Out does not need to be scala types, they can be generics (i.e. using shapeless like types)
  // In => Out
  // more advanced
  // Facts[("age", Int)] with Facts[("name", String)]
  // wan tto put the inside
  // Facts[("age", Int) with Facts[("name", String)]]
  // Facts[In] => Out
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

    def >=[In1 <: In, Out1 >: Out](that: Expr[In1, Out1])(implicit ev: PrimitiveType[Out1]): Expr[In1, Boolean] =
      !(self < that) || (self === that)

    def ifTrue[In1 <: In, Out2](
      ifTrue: Expr[In1, Out2]
    )(implicit
      ev: Out <:< Boolean
    ): Expr.IfTrue[In1, Out2] = Expr.IfTrue(self.widen[Boolean], ifTrue)

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

    final case class Constant[Out](value: Out, tag: PrimitiveType[Out])         extends Expr[Any, Out]
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

    implicit def apply[Out](out: Out)(implicit tag: PrimitiveType[Out]): Expr[Any, Out] = Constant(out, tag)

    // Introducer for Expr, we also need an eliminator
    //def input[A](implicit tag: PrimitiveType[A]): Expr[A, A] = Input(tag)
    def input[K <: Singleton with String, V](factDef: FactDefinition.KeyValue[K, V]): Expr[(K, V), V] =
      Input(factDef)

    def fact[In, K <: Singleton with String, V](
      factDef: FactDefinition.KeyValue[K, V],
      value: Expr[In, V]
    ): Expr[In, Facts[(K, V)]] =
      Fact(factDef, value)

    def ifThenElse[In, Out](
      condition: Expr[In, Boolean],
      ifTrue: Expr[In, Out],
      ifFalse: Expr[In, Out]
    ): Expr[In, Out] =
      IfThenElse(condition, ifTrue, ifFalse)

    // def field[In, Out](name: String)(implicit tag: PrimitiveType[Out]): Expr[In, Out] = UnsafeField(name, tag)
    //def unsafeField = ???

    sealed trait NumericBinOpType
    object NumericBinOpType {
      case object Add extends NumericBinOpType
      case object Sub extends NumericBinOpType
      case object Mul extends NumericBinOpType
      case object Div extends NumericBinOpType
    }
  }

  final case class Condition[-In](expr: Expr[In, Boolean]) { self =>
    def &&[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(self.expr && that.expr)

    def ||[In1 <: In](that: Condition[In1]): Condition[In1] =
      Condition(self.expr || that.expr)

    def unary_! : Condition[In] = Condition(!self.expr)

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

  def constrainedPolymorphicFunction[A](a: A)(implicit pt: PrimitiveType[A]): A = a

  // In => List[Out]
  // Out are fine grained instructions telling the engine what to do
  // Deal with structural data generically (i.e. shapeless, zio.schema)
  trait Action[-In, +Out] { self =>

    final def ++[In1 <: In, Out1 >: Out](that: Action[In1, Out1]): Action[In1, Out1] =
      Action.Concat(self, that)
    //Action(in => self.update(in) ++ that.update(in))

    final def >>>[Out2](that: Action[Out, Out2]): Action[In, Out2] =
      Action.Pipe(self, that)

    //Action(in => update(in).flatMap(that.update))

  }
  object Action {
    final case class Concat[In, Out](
      left: Action[In, Out],
      right: Action[In, Out]
    ) extends Action[In, Out]

    final case class Pipe[In, Out1, Out2](
      left: Action[In, Out1],
      right: Action[Out1, Out2]
    ) extends Action[In, Out1 with Out2]

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
    //def constant[Out](out: Out)(implicit pt: PrimitiveType[Out]): Action[Any, Out] = Constant(out, pt)

    def fromExpr[In, Out](expr: Expr[In, Out]): Action[In, Out] = FromExpr(expr)
  }

  // sealed trait RuntimeTypeTag[T]
  // object RuntimeTypeTag {
  //   implicit case object ByteTag   extends RuntimeTypeTag[Byte]
  //   implicit case object ShortTag  extends RuntimeTypeTag[Short]
  // }
  // We constrain the polymorphism to a set of types
  // We enforce the type at compile time and wire it with the RuntimeTypeTag[T]
  // final case class ConstantOld[Out](value: Out, runtimeTypeTag[Out]) extends Action[Any, Out]
  //ConstantOld("foo", runtimeTypeTag[String])")

  object loalty {

    object FlightBooking {
      val id       = FactDefinition.string("id")
      val customer = FactDefinition.string("customer")
      val flight   = FactDefinition.string("flight")
      val price    = FactDefinition.double("price")
      val status   = FactDefinition.string("status")
    }

    object FlightBookingStatus {
      val Confirmed = Expr("Confirmed")
      val Flight    = Expr("Flight")
      val Pending   = Expr("Pending")
    }

    val expr = Expr(
      10.0
    )

    val test1 =
      Condition(FlightBooking.status.get === FlightBookingStatus.Confirmed)

    val test2 =
      Condition(FlightBooking.price.get < expr)

    val test3 =
      Condition(FlightBooking.price.get < 1000.0)

    val facts  = FlightBooking.price.set(10000.0)
    val facts2 = FlightBooking.status.set(FlightBookingStatus.Confirmed)

    val f3 = facts ++ facts2

    val combo = test1 && test2

    /*
    val statusCondition1: Condition[String] =
      Condition.isEqualTo[String]("confirmed") //FlightBookingStatus.Confirmed)

    val priceCondition1: Condition[Double] =
      Condition.isGreaterThan[Double](100.0)

    val combinedExample = statusCondition1 && priceCondition1

    val test = combinedExample.expr
     */

    /* Example using the unsafe way to extract values
    val unsafeUnTyped1 =
      Condition(Expr.field[FlightBooking, String]("status") === Expr("Confirmed"))

    val unsafeUnTyped2 =
      Condition(Expr.field[FlightBooking, Double]("price") === Expr(100.0))

     */
    /* Using contramap did not work.
    val statusCondition1: Condition[String] =
      Condition.isEqualTo[String]("confirmed") //FlightBookingStatus.Confirmed)

    val priceCondition1: Condition[Double] =
      Condition.isGreaterThan[Double](100.0)

    val statusCondition: Condition[FlightBooking] =
      Condition
        .isEqualTo[String]("confirmed")
        .contramap[FlightBooking](Getter.Field("status")) //FlightBookingStatus.Confirmed)

    val priceCondition: Condition[FlightBooking] =
      Condition.isGreaterThan[Double](100.0).contramap[FlightBooking](Getter.Field("price"))

    val combinedExample = statusCondition && priceCondition
     */
  }
}
