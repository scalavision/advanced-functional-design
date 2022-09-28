package net.degoes.afd.rules

import zio._

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

object FactDefinition {
  type KeyValue[K <: Singleton with String, V] = FactDefinition[(K, V)] { type Key = K; type Value = V }

  def apply[N <: Singleton with String, T](name0: N, tag0: EngineType[T]): KeyValue[N, T] =
    new FactDefinition[(N, T)] {
      type Key   = N
      type Value = T
      def name: N            = name0
      def tag: EngineType[T] = tag0
    }

  def facts[N <: Singleton with String, Fields](name: N, factsType: FactsType[Fields]): KeyValue[N, Facts[Fields]] =
    FactDefinition[N, Facts[Fields]](name, EngineType.Composite(factsType))

  def prim[N <: Singleton with String, T](name0: N)(implicit tag0: PrimitiveType[T]): KeyValue[N, T] =
    new FactDefinition[(N, T)] {
      type Key   = N
      type Value = T
      def name: N            = name0
      def tag: EngineType[T] = EngineType.Primitive(tag0)
    }

  def boolean[N <: Singleton with String](name0: N): KeyValue[N, Boolean] = FactDefinition.prim[N, Boolean](name0)

  def byte[N <: Singleton with String](name0: N): KeyValue[N, Byte] = FactDefinition.prim[N, Byte](name0)

  def char[N <: Singleton with String](name0: N): KeyValue[N, Char] = FactDefinition.prim[N, Char](name0)

  def int[N <: Singleton with String](name0: N): KeyValue[N, Int] = FactDefinition.prim[N, Int](name0)

  def long[N <: Singleton with String](name0: N): KeyValue[N, Long] = FactDefinition.prim[N, Long](name0)

  def float[N <: Singleton with String](name0: N): KeyValue[N, Float] = FactDefinition.prim[N, Float](name0)

  def double[N <: Singleton with String](name0: N): KeyValue[N, Double] = FactDefinition.prim[N, Double](name0)

  def string[N <: Singleton with String](name0: N): KeyValue[N, String] = FactDefinition.prim[N, String](name0)

  def instant[N <: Singleton with String](name0: N): KeyValue[N, java.time.Instant] =
    FactDefinition.prim[N, java.time.Instant](name0)
}

sealed abstract case class Facts[+Types] private (private val data: Map[FactDefinition[_], Any]) { self =>
  def ++[Types2](that: Facts[Types2]): Facts[Types & Types2] =
    new Facts[Types & Types2](data ++ that.data) {}

  def definitions: Chunk[FactDefinition[_]] = Chunk.fromIterable(data.keys)

  def get[Key <: Singleton with String, Value: PrimitiveType](pd: FactDefinition[(Key, Value)])(implicit
    subset: Types <:< (Key, Value)
  ): Value =
    data(pd).asInstanceOf[Value]

  /**
   * Returns a new facts collection with the specified primitive fact added.
   */
  def add[Key <: Singleton with String, Value](
    pd: FactDefinition.KeyValue[Key, Value],
    value: Value
  ): Facts[Types & (Key, Value)] =
    new Facts[Types & (Key, Value)](data + (pd -> value)) {}

  /**
   * Returns a new facts collection with the specified composite fact added.
   */
  def add[Key <: Singleton with String, Types2](
    pd: FactDefinition.KeyValue[Key, Facts[Types2]],
    value: Facts[Types2]
  ): Facts[Types & (Key, Facts[Types2])] =
    new Facts[Types & (Key, Facts[Types2])](data + (pd -> value)) {}

  object unsafe {
    def get(pd: FactDefinition[_])(implicit unsafe: Unsafe): Option[Any] = data.get(pd)
  }
}

object Facts {

  /**
   * An empty facts collection.
   */
  val empty: Facts[Any] = new Facts[Any](Map.empty) {}
}
