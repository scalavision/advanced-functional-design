package net.degoes.afd.rules

import zio.&

object Example {

  object Customer {
    val id    = FactDefinition.string(("id"))
    val name  = FactDefinition.string(("name"))
    val email = FactDefinition.string(("email"))
    val phone = FactDefinition.string(("phone"))
    val factsType =
      FactsType.empty.add(id).add(name).add(email).add(phone)
  }

  object Flight {
    val id          = FactDefinition.string(("id"))
    val origin      = FactDefinition.string(("origin"))
    val destination = FactDefinition.string(("destination"))
    val factsType =
      FactsType.empty.add(id).add(origin).add(destination)
  }

  object FlightBookingStatus {
    val Confirmed = Expr("confirmed")
    val Cancelled = Expr("cancelled")
    val Pending   = Expr("pending")
    //val factsType = FactsType.empty.add(Confirmed).add(Cancelled).add(Pending)
  }

  object FlightBooking {
    val id       = FactDefinition.string("id")
    val customer = FactDefinition.facts("customer", Customer.factsType)
    val flight   = FactDefinition.facts("flight", Flight.factsType)
    val price    = FactDefinition.double("price")
    val status   = FactDefinition.string("status")
    val factsType =
      FactsType.empty.add(id).add(customer).add(price).add(status)
  }

  val statusCondition =
    Condition(FlightBooking.status.get === FlightBookingStatus.Pending)

  val priceCondition =
    Condition(FlightBooking.price.get >= 1000.0)

  val both: Condition[Facts[("status", String)] with Facts[("price", Double)]] =
    statusCondition && priceCondition

  val newPrice = FlightBooking.price.set(Expr(1000.0))

  object LoyaltyAction {
    val actionType = FactDefinition.string("action_type")
    val points     = FactDefinition.int("points")
    val customer   = FactDefinition.string("customer")
  }

  object ActionType {
    val AddPoints     = Expr("add_points")
    val UpgradeTier   = Expr("upgrade_tier")
    val DowngradeTier = Expr("downgrade_tier")
  }

  val rule: Rule[Facts[("status", String)] with Facts[("price", Double)], Facts[("action_type", String)]] = Rule(
    both,
    Action.fromExpr {
      (LoyaltyAction.actionType := ActionType.AddPoints) ++
        (LoyaltyAction.points   := 100)
    } ++
      Action.fromExpr {
        (LoyaltyAction.actionType := ActionType.UpgradeTier)
      }
  )
}
