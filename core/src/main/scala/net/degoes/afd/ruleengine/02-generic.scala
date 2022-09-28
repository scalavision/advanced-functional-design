/**
 * The next step in the evolution of a rule engine is to make it generic, across
 * all domains (not just an airline loyalty program).
 *
 * In this section, you will explore different ways to encode a generic rule
 * engine, and discover more powerful functional domains in the process.
 */
package net.degoes.afd.ruleengine

/**
 * Create a functional domain to express how rules translate into actions, which
 * can be leveraged across multiple business domains (loyalty points,
 * recommendations, customer onboarding, and event processing).
 *
 * For simplicity and ease-of-construction, use the executable encoding, but be
 * sure to leverage parametric polymorphism in your efforts to make the rule
 * engine generic.
 */
// In2, Out2 are new types
// In1, Out1 are subtypes
object generic {
  /*
  //trait RuleEngine
  final case class RuleEngine[-In, +Out](update: In => Option[List[Out]]) { self =>

    def >>>[Out2](that: RuleEngine[Out, Out2]): RuleEngine[In, Out2] =
      RuleEngine(in => self.update(in).flatMap(that.update))

    def zip[In1 <: In, Out2](that: RuleEngine[In1, Out2]): RuleEngine[In1, (Out, Out2)] =
      RuleEngine(in => self.update(in).zip(that.update(in)))

    def updateWith(in: In)(defaultOut: Out, combine: (Out, Out) => Out): Out =
      self.update(in) match {
        case None       => defaultOut
        case Some(outs) => outs.reduceOption(combine).getOrElse(defaultOut)
      }
  }

  //.getOrElse(List(defaultOut)).reduce(combine)

      RuleEngine(in =>
        (self.update(in), that.update(in)) match {
          case (Some(out1), Some(out2)) => Some((out1, out2))
          case _                        => None
        }
      )

  //RuleEngine(in => self.update(in).zip(that.update(in)))

  def contramap[In2](f: In2 => In): RuleEngine[In2, Out] =
    RuleEngine(in2 => self.update(f(in2)))

  def flatMap[In1 <: In, Out2](f: Out => RuleEngine[In1, Out2]): RuleEngine[In1, Out2] =
    RuleEngine(in => self.update(in).flatMap(out => f(out).update(in)))

  def map[Out2](f: Out => Out2): RuleEngine[In, Out2] =
    RuleEngine(in => self.update(in).map(f))

  // fallback for the rules engine
  def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
    RuleEngine(in => update(in).orElse(that.update(in)))
}

object RuleEngine {

  def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] =
    RuleEngine(ruleSet.update)

  def collect[In, Out](rules: List[Rule[In, Out]]): RuleEngine[In, Out] =
    RuleEngine(in => rules.collectFirst { case rule if rule.condition.eval(in) => rule.action.update(in) })

  def constant[Out2](out2: Out2): RuleEngine[Any, Out2] =
    RuleEngine(_ => Some(out2))
}

//trait RuleSet
final case class RuleSet[-In, +Out](rules: Vector[Rule[In, Out]]) {
  self =>
  i
//
  def ++[In1 <: In, Out1 >: Out](that: RuleSet[In1, Out1]): RuleSet[In1, Out1] = RuleSet(self.rules ++ that.rules)

  def +[In1 <: In, Out1 >: Out](rule: Rule[In1, Out1]): RuleSet[In1, Out1] = RuleSet(self.rules :+ rule)

  def update[In1 <: In](in: In1): Option[List[Out]] =
    self.rules
      .find(
        _.condition
          .eval(in)
      )
      .map { rule =>
        rule.action.update(in)
      }
}

object RuleSet {
  def apply[In, Out](rule1: Rule[In, Out], rules: Rule[In, Out]*): RuleSet[In, Out] =
    RuleSet(rule1 +: rules.toVector)

  val empty: RuleSet[Any, Nothing] = RuleSet(Vector.empty)
}

//trait Rule
final case class Rule[-In, +Out](
  condition: Condition[In],
  action: Action[In, Out]
)

//  trait Condition
final case class Condition[-In](eval: In => Boolean) {
  self =>
  def contramap[In2](f: In2 => In): Condition[In2] =
    Condition(f.andThen(self.eval))

  def &&[In1 <: In](that: Condition[In1]): Condition[In1] = Condition(in => eval(in) && that.eval(in))
  def ||[In1 <: In](that: Condition[In1]): Condition[In1] = Condition(in => eval(in) || that.eval(in))
  def unary_![In1 <: In]: Condition[In1]                  = Condition(in => !eval(in))
}

object Condition {
  def always: Condition[Any]                            = Condition(_ => true)
  def never: Condition[Any]                             = Condition(_ => false)
  def constant[In](value: Boolean): Condition[In]       = Condition(_ => value)
  def fromFunciton[In](f: In => Boolean): Condition[In] = Condition(f)
}

// trait Action
// -In, scala will have a default type member of Any
// +Out, scala will have a default type member of Nothing
// This makes type inference work way better
final case class Action[-In, +Out](update: In => List[Out]) { self =>

  def ++[In1 <: In, Out1 >: Out](that: Action[In, Out]): Action[In1, Out1] =
    Action(in => self.update(in) ++ that.update(in))

  def >>>[Out2](
    that: Action[Out, Out2]
  ): Action[In, Out2] =
    Action(in => self.update(in).flatMap(out => that.update(out)))

  def flatMap[In1 <: In, Out2](
    f: Out => Action[In1, Out2]
  ): Action[In1, Out2] =
    Action(in => f(self.update(in)).update(in))

  def map[Out2](
    f: Out => Out2
  ): Action[In, Out2] =
    self.flatMap(out => Action.constant(f(out)))
  //Action.fromFunction(f) >>> self

  def contramap[In2](f: In2 => In): Action[In2, Out] =
    Action(in2 => self.update(f(in2)))

  def zip[In1 <: In, Out2](
    that: Action[In1, Out2]
  ): Action[In1, (Out, Out2)] =
    self.flatMap(out => that.map(out2 => (out, out2)))
}

object Action {

  def constant[Out](out: Out): Action[Any, Out] = Action(_ => out)

  def fromFunction[In, Out](f: In => Out): Action[In, Out] = Action(f)

  // We can easily narrow down the type of the input
  def intComment: Action[String, Int] = constant(42)

}

object loyalty {

  import net.degoes.afd.examples.loyalty._

  import net.degoes.afd.examples.loyalty.FlightBookingStatus
  import net.degoes.afd.examples.loyalty.FlightBookingStatus._

  import net.degoes.afd.examples.loyalty.LoyaltyTier._

  type Patch[A] = A => A
  //type LoyaltyEngine = RuleEngine[(FlightBooking, LoyaltyProgram), LoyaltyProgram]
  type LoyaltyEngine =
    RuleEngine[FlightBooking, Patch[LoyaltyProgram]]

  type LoyaltyRuleSet = RuleSet[FlightBooking, Patch[LoyaltyProgram]]

  object LoyaltyRuleSet {
    def empty: LoyaltyRuleSet =
      RuleSet.empty //[(FlightBooking, LoyaltyProgram), LoyaltyProgram]
  }

  type LoyaltyRule = Rule[FlightBooking, Patch[LoyaltyProgram]]

  object LoyaltyRule {
    def apply(
      condition: LoyaltyCondition,
      action: LoyaltyAction
    ): LoyaltyRule = Rule(condition, action)

  }

  type LoyaltyCondition = Condition[FlightBooking]
  object LoyaltyCondition {
    def status(f: FlightBookingStatus => Boolean): LoyaltyCondition = Condition(f compose (_.status))

    def price(f: Double => Boolean): LoyaltyCondition = Condition(f compose (_.price))
    //Condition[LoyaltyCondition](_.price == p)
  }
  //trait LoyaltyAction
  type LoyaltyAction = Action[Any, Patch[LoyaltyProgram]]

  object LoyaltyAction {

    val upgradeTier: LoyaltyAction = ???

    LoyaltyAction(loyaltyProgram =>
        loyaltyProgram.copy(tier = loyaltyProgram.tier match {
          case Bronze => Silver
          case Silver => Gold
          case Gold   => Gold
        })
      )

    def adjustPoints(points: Int): LoyaltyAction = Action[LoyaltyProgram, LoyaltyProgram](loyaltyProgram =>
      loyaltyProgram.copy(points = loyaltyProgram.points + points)
    )
  }

  val exampleAction = LoyaltyAction.upgradeTier //++ LoyaltyAction.addPoints(100)
  val exampleCondition =
    LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) &&
      LoyaltyCondition.price(_ > 1000)

  val exampleRule    = LoyaltyRule(exampleCondition, LoyaltyAction.adjustPoints(100))
  val exampleRuleSet = ??? //LoyaltyRuleSet.empty.addRule(exampleRule)

  val engine = RuleEngine.fromRuleSet(exampleRuleSet)

  def updateLoyaltyProgram(
    booking: FlightBooking,
    program: LoyaltyProgram
  ): LoyaltyProgram = {
    val identityPatch = identity[LoyaltyProgram](_)
    val patch         = engine.updateWith(booking)(empty, _ andThen _)
    patch(program)
  }
  //engine.update((booking, program)).getOrElse(program)

}

object actionExamples {
  import generic._
  // Action is a function that takes an input and returns an outputi
  trait Animal
  trait Dog extends Animal
  trait Cat extends Animal

  def bar(action: Action[Dog, _]) = ???
  // We can narrow down the type of the input
  def foo(action: Action[Animal, _]) = ??? // bar(action.narrow[Dog](dog => dog: Animal))

  // A contravariant input can always be narrowed, all the way down to
  // Nothing, but you will never be able to call this function
  def testRuleCombine(
    l: Rule[Any, Cat],
    r: Rule[String, Dog]
  ): RuleSet[String, Animal] =
    RuleSet(l) ++ RuleSet(r)

  // This is safe, because Int and String are final, nothing can inherit from these.
  def testRuleCombine2(
    l: Rule[Int, Cat],
    r: Rule[String, Dog]
  ): RuleSet[Int with String, Animal] =
    RuleSet(l) ++ RuleSet(r)
}*/
}
