package net.degoes.afd.rules

final case class RuleEngine[-In, +Out](update: In => Option[List[Out]]) { self =>

  def contramap[In2](f: In2 => In): RuleEngine[In2, Out] =
    RuleEngine((in2: In2) => self.update(f(in2)))

  def orElse[In1 <: In, Out1 >: Out](that: RuleEngine[In1, Out1]): RuleEngine[In1, Out1] =
    RuleEngine((in: In1) => self.update(in) orElse that.update(in))

  def updateWith[Out1 >: Out](in: In)(defaultOut: Out1, combine: (Out1, Out1) => Out1): Out1 =
    self.update(in) match {
      case None => defaultOut

      case Some(outs) =>
        outs.reduceOption(combine).getOrElse(defaultOut)
    }

  def fromFunction[In, Out](f: In => Out): RuleEngine[In, Out] =
    RuleEngine((in: In) => Some(List(f(in))))

}
object RuleEngine {

  val empty: RuleEngine[Any, Nothing] = RuleEngine(_ => None)

  def constant[Out](out: Out): RuleEngine[Any, Out] = fromFunction(_ => out)

  def apply[In, Out](f: In => Option[List[Out]]): RuleEngine[In, Out] =
    new RuleEngine(f)

  def collect[In, Out](pf: PartialFunction[In, Out]): RuleEngine[In, Out] =
    RuleEngine(in => pf.lift(in).map(List(_)))

  def fromFunction[In, Out](f: In => Out): RuleEngine[In, Out] = RuleEngine(in => Some(List(f(in))))

  def fromRuleSet[In, Out](ruleSet: RuleSet[In, Out]): RuleEngine[In, Out] = {
    val update: In => Option[List[Out]] = execute(ruleSet, _)
    RuleEngine(update)
  }

  private def execute[In, Out](ruleSet: RuleSet[In, Out], in: In): Option[List[Out]] =
    ruleSet.rules.find(_.condition.eval(in)).map { rule =>
      // gives a list of Out
      rule.action.eval(in)
    }

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
