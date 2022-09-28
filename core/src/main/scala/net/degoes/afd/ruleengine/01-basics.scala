/**
 * The simplest of all functional domains uses untyped models that are highly
 * specialized, encoded using the executable encoding.
 *
 * In this section, you will explore the domain of rule engines by making the
 * simplest possible, domain-specialized, untyped functional model, equipped
 * with constructors and operators.
 */
package net.degoes.afd.ruleengine

import net.degoes.afd.examples.loyalty._
import net.degoes.afd.examples.loyalty.LoyaltyTier._

/**
 * Create a functional domain to express how customer actions translate into
 * loyalty points and tier changes.
 *
 * For simplicity and ease-of-construction, use the executable encoding and an
 * untyped functional model.
 *
 * Attempt to make each subdomain as compositional as possible, with
 * constructors, binary operators, and unary operators.
 */
object basics {

  /**
   * Update loyalty program based upon a new flight booking Responsible for
   * computation in response to a booking that results in
   */
  final case class LoyaltyEngine(
    update: (FlightBooking, LoyaltyProgram) => LoyaltyProgram
  )

  object LoyaltyEngine {
    def fromRuleSet(ruleSet: LoyaltyRuleSet): LoyaltyEngine =
      LoyaltyEngine((booking, program) =>
        ruleSet.rules
          .find(
            _.condition
              .eval(booking)
          )
          .map { rule =>
            rule.action.update(program)
          }
          .getOrElse(program)
      )

  }

  /*
      ruleSet.rules.foldLeft(LoyaltyEngine(identity)) { case (engine, rule) =>
        if(rule.condition.eval(booking)) rules.action.update(program)
        else program
        //engine.andThen(rule)
      }*/
  //ruleSet.update(booking, program))
  //LoyaltyEngine(ruleSet.update)
  /**
   * Idea, combine a bunch of rules into a rule set
   */
  // trait LoyaltyRuleSet
  final case class LoyaltyRuleSet(rules: Vector[LoyaltyRule]) { self =>
    def +(rule: LoyaltyRule): LoyaltyRuleSet             = LoyaltyRuleSet(rules :+ rule)
    def addRule(rule: LoyaltyRule): LoyaltyRuleSet       = self + rule
    def ++(that: LoyaltyRuleSet): LoyaltyRuleSet         = LoyaltyRuleSet(rules ++ that.rules)
    def addRuleSet(that: LoyaltyRuleSet): LoyaltyRuleSet = self ++ that
  }

  object LoyaltyRuleSet {
    def empty: LoyaltyRuleSet                  = LoyaltyRuleSet(Vector.empty)
    def fromFile(path: String): LoyaltyRuleSet = ???
  }

  //trait LoyaltyRule
  final case class LoyaltyRule(condition: LoyaltyCondition, action: LoyaltyAction) {
    def ++(that: LoyaltyRule): LoyaltyRule =
      LoyaltyRule(condition && that.condition, action ++ that.action)
  }

  //final case class LoyaltyRule(f: FlightBooking => Boolean, a: LoyaltyAction)
  // final case class LoyaltyRule(f: FlightBooking => LoyaltyAction)

  /**
   * Testing if price > 1000 is typically a condition Executing some logic based
   * upon the condition
   */
  //trait LoyaltyCondition
  final case class LoyaltyCondition(eval: FlightBooking => Boolean) {
    def &&(that: LoyaltyCondition): LoyaltyCondition =
      LoyaltyCondition(booking => eval(booking) && that.eval(booking))
    def ||(that: LoyaltyCondition): LoyaltyCondition =
      LoyaltyCondition(booking => eval(booking) || that.eval(booking))
    def unary_! : LoyaltyCondition =
      LoyaltyCondition(booking => !eval(booking))
  }

  object LoyaltyCondition {
    val always: LoyaltyCondition = LoyaltyCondition(_ => true)
    val never: LoyaltyCondition  = LoyaltyCondition(_ => false)
    def constant(value: Boolean): LoyaltyCondition =
      LoyaltyCondition(_ => value)

    def status(p: FlightBookingStatus => Boolean): LoyaltyCondition =
      LoyaltyCondition(booking => p(booking.status))

    def price(p: Double): LoyaltyCondition =
      LoyaltyCondition(_.price == p)

    val exampleRule      = ???
    val exampleRuleSet   = LoyaltyRuleSet.empty.addRule(???)
    val exampleCondition = LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) && LoyaltyCondition.price(1000)

    val exampleAction = LoyaltyAction.upgradeTier ++ LoyaltyAction.adjustPoints(100)

    /*
    def points(points: Int => Boolean): LoyaltyCondition =
      LoyaltyCondition(_.loyaltyProgram.points == points)
     */
  }

  /*
   * Response to a loyalty action
  sealed trait LoyaltyAction { self =>
    import LoyaltyAction._
    def ++(that: LoyaltyAction): LoyaltyAction = Both(self, that)
  }
  object LoyaltyAction {
    def adjustPoints(value: Int): LoyaltyAction = adjustPoints(value)
    val none: LoyaltyAction                     = Unchanged
    private[basics] final case object Unchanged                                 extends LoyaltyAction
    private[basics] final case object UpgradeTier                               extends LoyaltyAction
    private[basics] final case class AddPoints(points: Int)                     extends LoyaltyAction
    private[basics] final case object DowngradeTier                             extends LoyaltyAction
    private[basics] final case class Both(l1: LoyaltyAction, l2: LoyaltyAction) extends LoyaltyAction

  }

  trait LoyaltyAction {
    def update(oldProgram: LoyaltyProgram): LoyaltyProgram
    def serialize(oldProgram: LoyaltyProgram): JSON
    def deserialize(oldProgram: LoyaltyProgram): Either[Throwable, LoyaltyProgram]
  }
   */

  final case class LoyaltyAction(update: LoyaltyProgram => LoyaltyProgram) { self =>
    def ++(that: LoyaltyAction): LoyaltyAction =
      LoyaltyAction(self.update.andThen(that.update))
  }

  object LoyaltyAction {
    def adjustPoints(value: Int): LoyaltyAction = LoyaltyAction { program =>
      program.copy(points = program.points + value)
    }

    def downgradeTier: LoyaltyAction = LoyaltyAction(program =>
      program.copy(tier = program.tier match {
        case Bronze => Bronze
        case Silver => Bronze
        case Gold   => Silver
      })
    )
    def none: LoyaltyAction = LoyaltyAction(identity)
    def upgradeTier: LoyaltyAction = LoyaltyAction(program =>
      program.copy(tier = program.tier match {
        case Bronze => Silver
        case Silver => Gold
        case Gold   => Gold
      })
    )
  }

  object example {
    val exampleCondition = LoyaltyCondition.status(_ == FlightBookingStatus.Confirmed) && LoyaltyCondition.price(1000)
    val exampleRule      = LoyaltyRule(exampleCondition, LoyaltyAction.adjustPoints(100))
    val exampleRuleSet   = LoyaltyRuleSet.empty.addRule(exampleRule)
  }

}
