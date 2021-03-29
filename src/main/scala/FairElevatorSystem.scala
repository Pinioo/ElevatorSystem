package org.pinowski.elevatorsystem

class FairElevatorSystem(
                          els: Vector[Elevator]
                        ) extends ElevatorSystem(els) {
  override def callFromLevel(level: Int, direction: ElevatorDirection): ElevatorSystem = {
    this
  }

  override def callFromElevator(elevatorIndex: Int, level: Int): ElevatorSystem = {
    this
  }

  override def step(): ElevatorSystem =
    FairElevatorSystem(elevators.map(_.step()))

  def elevatorsGoingDown: Vector[Elevator] =
    elevators.filter{
      case Elevator(level, l +: nextLevel +: _) if
        l == level && level > nextLevel => true
      case Elevator(level, nextLevel +: _) if
        level > nextLevel => true
      case _ => false
    }

  def elevatorsGoingUp: Vector[Elevator] =
    elevators.filter {
      case Elevator(level, l +: nextLevel +: _) if
        l == level && level < nextLevel => true
      case Elevator(level, nextLevel +: _) if
        level < nextLevel => true
      case _ => false
    }

  def elevatorsNotGoing: Vector[Elevator] =
    elevators.filter {
      case Elevator(_, Vector()) => true
      case Elevator(level, Vector(nextLevel)) if level == nextLevel => true
      case _ => false
    }
}

object FairElevatorSystem {
  def apply(els: Vector[Elevator]) = new FairElevatorSystem(els)
}

