package org.pinowski.elevatorsystem

/**
 * Representation of elevator's state
 * @param floor current floor
 * @param goals floors the elevator is heading (in order)
 * @param direction optional explicit direction of the elevator
 *                  None if not required, else Some(Option[ElevatorDirection])
 */
case class Elevator(
                     floor: Int,
                     goals: Vector[Int],
                     direction: Option[Option[ElevatorDirection]] = None
                   ) {
  def step(): Elevator = {
    goals match {
      case this.floor +: gs => Elevator(floor, gs, direction)
      case _ => Elevator(floor + nextLevelChange, goals, direction)
    }
  }

  def getDirection: Option[ElevatorDirection] =
    direction.getOrElse {
      nextLevelChange match {
        case -1 => Option(ElevatorDown)
        case 1 => Option(ElevatorUp)
        case _ => None
      }
    }

  def nextLevelChange: Int =
    goals match {
      case g +: _ => (g - floor).sign
      case _ => 0
    }

  def updatedGoals(newGoals: Vector[Int]): Elevator =
    Elevator(floor, newGoals, direction)

  def updatedDirection(newDirection: Option[ElevatorDirection]): Elevator =
    Elevator(floor, goals, Option(newDirection))
}
