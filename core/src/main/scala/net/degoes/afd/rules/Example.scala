package net.degoes.afd.rules

object Example {

  object FlightBooking {
    val id = FactDefinition.string("id")

    val customer = FactDefinition.string("customer")
    val price    = FactDefinition.double("price")
    val status   = FactDefinition.string("status")
  }

  object FlightBookingStatus {
    val Confirmed = Expr("confirmed")
    val Cancelled = Expr("cancelled")
    val Pending   = Expr("pending")
  }

  val statusCondition =
    Condition(FlightBookingStatus.Confirmed === FlightBookingStatus.Pending)

  val priceCondition =
    Condition(FlightBooking.price.get >= 1000.0)
  //Condition(FlightBooking.price.get >= Expr(1000.0))

  val both: Condition[("status", String) with ("price", Double)] =
    statusCondition && priceCondition

  val newPrice = FlightBooking.price.set(Expr(1000.0))

  val newPrice2 = (FlightBooking.price := 1000.0) ++
    (FlightBooking.customer := "John Doe") ++
    (FlightBooking.status   := FlightBookingStatus.Confirmed)

  /*
  val flightBookingState =
    newPrice2 ++
      FlightBooking.customer := "John Doe" ++
      FlightBooking.status   := "confirmed"
   */
}
