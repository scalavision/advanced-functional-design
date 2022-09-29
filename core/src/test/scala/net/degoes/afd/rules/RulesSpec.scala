package net.degoes.afd.rules

import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

object RulesSpec extends ZIOSpecDefault {

  private def d[A](a: A): Unit = pprint.pprintln(a)

  val name: FactDefinition.KeyValue["name", String] =
    FactDefinition.string("name")

  val age: FactDefinition.KeyValue["age", Int] =
    FactDefinition.int("age")

  def spec =
    suite("RulesSpec") {
      test("EngineType") {
        val ets = EngineType.fromPrimitive[String]
        assert(ets)(equalTo(EngineType.Primitive[String](PrimitiveType.StringType)))
      }
      test("factDefinitions") {
        val fact = FactsType.empty

        val allFactDefs: FactsType[("name", String) with ("age", Int) with Any] =
          fact.add(name).add(age)

        assert(allFactDefs)(!equalTo(FactsType.empty.add(name).add(age)))
      }
      test("Facts") {
        val facts      = Facts.empty
        val namedFacts = facts.add(name, "John Doe")
        assert("John Doe")(equalTo(namedFacts.get(name)))
      }
    }
}
