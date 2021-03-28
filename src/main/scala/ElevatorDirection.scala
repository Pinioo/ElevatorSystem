package org.pinowski.elevatorsystem

sealed trait ElevatorDirection

case object ElevatorUp extends ElevatorDirection
case object ElevatorDown extends ElevatorDirection

object ElevatorDirection {
    /**
      * "u" -> ElevatorUp
      *
      * "d" -> ElevatorDown 
      * @param directionStr input string with direction
      * @return Some[ElevatorDirection] if param is a valid direction string,
      * None otherwise
      */
    def apply(directionStr: String): Option[ElevatorDirection] = directionStr match {
        case "u" => Option(ElevatorUp)
        case "d" => Option(ElevatorDown)
        case _   => None
    }
}
