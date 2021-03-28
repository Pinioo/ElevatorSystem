package org.pinowski.elevatorsystem

/**
  * Class implementing simplest elevator system
  *
  * Every call is put at the end of goals list of some elevator
  * @param els vector of elevators' states
  * @param nextElevatorToUse internal parameter telling which elevator
  * should be given next task from callFromLevel
  */
class SimpleElevatorSystem(
                            els: Vector[Elevator],
                            nextElevatorToUse: Int = 0
                          ) extends ElevatorSystem(els) {
  override def callFromLevel(level: Int, direction: ElevatorDirection): ElevatorSystem = {
    val elevatorToWorkWith = elevators(nextElevatorToUse)
    val newElevatorState = elevatorToWorkWith.updatedGoals(elevatorToWorkWith.goals :+ level)
    SimpleElevatorSystem(
      elevators.updated(
        nextElevatorToUse,
        newElevatorState
      ),
      (nextElevatorToUse + 1) % elevators.size
    )
  }

  override def callFromElevator(elevatorIndex: Int, level: Int): ElevatorSystem = {
    val elevatorToWorkWith = elevators(elevatorIndex)
    val newElevatorState = elevatorToWorkWith.updatedGoals(elevatorToWorkWith.goals :+ level)
    SimpleElevatorSystem(
      elevators.updated(
        elevatorIndex,
        newElevatorState
      ),
      nextElevatorToUse
    )
  }

  override def step(): ElevatorSystem =
    SimpleElevatorSystem(elevators.map(_.step()))
}

object SimpleElevatorSystem {
  def apply(els: Vector[Elevator], next: Int = 0) = new SimpleElevatorSystem(els, next)
}