package net.degoes.afd.rules

import zio._
import scala.annotation.implicitNotFound

sealed trait Numeric[A] { self =>
  type NumericType = A
  import Expr.NumericBinOpType
  import Expr.NumericBinOpType._

  def add(left: A, right: A): A
  def subtract(left: A, right: A): A
  def multiply(left: A, right: A): A
  def divide(left: A, right: A): A
  def mod(left: A, right: A): A

  def primitiveType: PrimitiveType[A] = Numeric.toPrimitiveType(self)

  def apply(binOp: Expr.NumericBinOpType)(left: A, right: A): A =
    binOp match {
      case Add    => add(left, right)
      case Sub    => subtract(left, right)
      case Mul    => multiply(left, right)
      case Div    => divide(left, right)
      case Modulo => mod(left, right)
    }
}

object Numeric {
  implicit case object ByteIsNumeric extends Numeric[Byte] {
    def add(left: Byte, right: Byte): Byte      = (left + right).toByte
    def subtract(left: Byte, right: Byte): Byte = (left - right).toByte
    def multiply(left: Byte, right: Byte): Byte = (left * right).toByte
    def divide(left: Byte, right: Byte): Byte   = (left / right).toByte
    def mod(left: Byte, right: Byte): Byte      = (left % right).toByte
  }
  implicit case object CharIsNumeric extends Numeric[Char] {
    def add(left: Char, right: Char): Char      = (left + right).toChar
    def subtract(left: Char, right: Char): Char = (left - right).toChar
    def multiply(left: Char, right: Char): Char = (left * right).toChar
    def divide(left: Char, right: Char): Char   = (left / right).toChar
    def mod(left: Char, right: Char): Char      = (left % right).toChar
  }
  implicit case object IntIsNumeric extends Numeric[Int] {
    def add(left: Int, right: Int): Int      = left + right
    def subtract(left: Int, right: Int): Int = left - right
    def multiply(left: Int, right: Int): Int = left * right
    def divide(left: Int, right: Int): Int   = left / right
    def mod(left: Int, right: Int): Int      = left % right
  }
  implicit case object LongIsNumeric extends Numeric[Long] {
    def add(left: Long, right: Long): Long      = left + right
    def subtract(left: Long, right: Long): Long = left - right
    def multiply(left: Long, right: Long): Long = left * right
    def divide(left: Long, right: Long): Long   = left / right
    def mod(left: Long, right: Long): Long      = left % right
  }
  implicit case object FloatIsNumeric extends Numeric[Float] {
    def add(left: Float, right: Float): Float      = left + right
    def subtract(left: Float, right: Float): Float = left - right
    def multiply(left: Float, right: Float): Float = left * right
    def divide(left: Float, right: Float): Float   = left / right
    def mod(left: Float, right: Float): Float      = left % right
  }
  implicit case object DoubleIsNumeric extends Numeric[Double] {
    def add(left: Double, right: Double): Double      = left + right
    def subtract(left: Double, right: Double): Double = left - right
    def multiply(left: Double, right: Double): Double = left * right
    def divide(left: Double, right: Double): Double   = left / right
    def mod(left: Double, right: Double): Double      = left % right
  }

  def toPrimitiveType[A](num: Numeric[A]): PrimitiveType[A] = num match {
    case _: Numeric.ByteIsNumeric.type   => PrimitiveType.ByteType
    case _: Numeric.CharIsNumeric.type   => PrimitiveType.CharType
    case _: Numeric.IntIsNumeric.type    => PrimitiveType.IntType
    case _: Numeric.LongIsNumeric.type   => PrimitiveType.LongType
    case _: Numeric.FloatIsNumeric.type  => PrimitiveType.FloatType
    case _: Numeric.DoubleIsNumeric.type => PrimitiveType.DoubleType
  }
}

@implicitNotFound(
  "The type ${A} is not a supported Fact type, \nonly:\n\n Boolean, Byte, Short,Char,Int,Long,Float,Double,String,Instant \n\n are supported"
)
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

/**
 * A typeclass for types that can be converted to a [[PrimitiveType]].
 */
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

}
