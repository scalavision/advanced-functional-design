package net.degoes.afd.rules

import zio._

sealed trait Numeric[A] { self =>

  def primitiveType: PrimitiveType[A] =
    (self match {
      case _: Numeric.ByteIsNumeric.type   => PrimitiveType.ByteType
      case _: Numeric.CharIsNumeric.type   => PrimitiveType.CharType
      case _: Numeric.IntIsNumeric.type    => PrimitiveType.IntType
      case _: Numeric.LongIsNumeric.type   => PrimitiveType.LongType
      case _: Numeric.FloatIsNumeric.type  => PrimitiveType.FloatType
      case _: Numeric.DoubleIsNumeric.type => PrimitiveType.DoubleType
    }).asInstanceOf[PrimitiveType[A]]
}
object Numeric {
  implicit case object ByteIsNumeric   extends Numeric[Byte]
  implicit case object CharIsNumeric   extends Numeric[Char]
  implicit case object IntIsNumeric    extends Numeric[Int]
  implicit case object LongIsNumeric   extends Numeric[Long]
  implicit case object FloatIsNumeric  extends Numeric[Float]
  implicit case object DoubleIsNumeric extends Numeric[Double]
}

sealed trait PrimitiveType[A] { self =>
  def ordering: scala.math.Ordering[A] = PrimitiveType.orderingOf[A](self)
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

sealed trait EngineType[A]
object EngineType {
  final case class Primitive[A](primitiveType: PrimitiveType[A])   extends EngineType[A]
  final case class Composite[Fields](factsType: FactsType[Fields]) extends EngineType[Facts[Fields]]

  def fromPrimitive[A](implicit primType: PrimitiveType[A]): EngineType[A] =
    EngineType.Primitive(primType)

  def fromFacts[Types](facts: Facts[Types]): EngineType[Facts[Types]] =
    EngineType.Composite(FactsType.fromFacts(facts))
}

class FactsType[KeyValues] private (private val definitions: Chunk[FactDefinition[_]]) {
  def ++[KeyValues2](that: FactsType[KeyValues2]): FactsType[KeyValues & KeyValues2] =
    new FactsType(definitions ++ that.definitions)

  def add[KeyValue](definition: FactDefinition[KeyValue]): FactsType[KeyValue & KeyValues] =
    new FactsType(definitions :+ definition)
}

object FactsType {
  val empty: FactsType[Any] = new FactsType(Chunk.empty)

  def fromFacts[KeyValues](facts: Facts[KeyValues]): FactsType[KeyValues] =
    new FactsType(facts.definitions)

  sealed trait FactDefinition[KeyValue] { self =>
    type Key <: Singleton with String
    type Value

    def name: Key

    def tag: EngineType[Value]

    def get: Expr[(Key, Value), Value] = Expr.input(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]])

    def set[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] =
      Expr.fact(self.asInstanceOf[FactDefinition.KeyValue[Key, Value]], value)

    def :=[In](value: Expr[In, Value]): Expr[In, Facts[(Key, Value)]] = set(value)

    override final def toString(): String = s"FactDefinition($name, $tag)"
  }

}
