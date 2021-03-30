package org.pinowski.elevatorsystem

import scala.collection.immutable.TreeSet

class FairElevatorSystem(
                          els: Vector[Elevator],
                          downCalls: TreeSet[Int] = TreeSet.empty(Ordering.Int.reverse),
                          upCalls: TreeSet[Int] = TreeSet.empty
                        ) extends ElevatorSystem(els) {
  override def callFromLevel(level: Int, direction: ElevatorDirection): ElevatorSystem = {
    val elevatorOption: Option[Elevator] = direction match {
      case ElevatorDown =>
        (elevatorsGoingDown ++ elevatorsNotGoing)
          .filter(el => el.level >= level)
          .minOption(Ordering.by[Elevator, Int](el => el.level - level))
      case ElevatorUp =>
        (elevatorsGoingUp ++ elevatorsNotGoing)
          .filter(el => el.level <= level)
          .minOption(Ordering.by[Elevator, Int](el => level - el.level))
    }

    elevatorOption match {
      case Some(elevator) =>
        val elevatorIndex = elevators
          .zipWithIndex
          .find{case (el, _) => el == elevator}
          .map{case (_, ind) => ind}
          .get
        val newGoals = direction match {
          case ElevatorDown =>
            val elevatorsAbove = elevator
              .goals
              .takeWhile(_ > level)
            val elevatorsBelow = elevator
              .goals
              .dropWhile(_ >= level)
            elevatorsAbove ++ Vector(level) ++ elevatorsBelow
          case ElevatorUp =>
            val elevatorsBelow = elevator
              .goals
              .takeWhile(_ < level)
            val elevatorsAbove = elevator
              .goals
              .dropWhile(_ <= level)
            elevatorsBelow ++ Vector(level) ++ elevatorsAbove
        }
        FairElevatorSystem(elevators.updated(elevatorIndex, elevator.updatedGoals(newGoals)), downCalls, upCalls)
      case None =>
        direction match {
          case ElevatorDown =>
            FairElevatorSystem(elevators, downCalls + level, upCalls)
          case ElevatorUp =>
            FairElevatorSystem(elevators, downCalls, upCalls + level)
        }
    }

  }

  override def callFromElevator(elevatorIndex: Int, level: Int): ElevatorSystem = {
    this
  }

  override def step(): ElevatorSystem =
    FairElevatorSystem(elevators.map(_.step()))

  val elevatorsGoingDown: Vector[Elevator] =
    elevators.filter{
      case Elevator(level, nextLevel +: nextNextLevel +: _) if
        level == nextLevel && level > nextNextLevel => true
      case Elevator(level, nextLevel +: _) if
        level > nextLevel => true
      case _ => false
    }

  val elevatorsGoingUp: Vector[Elevator] =
    elevators.filter {
      case Elevator(level, nextLevel +: nextNextLevel +: _) if
        level == nextLevel && level < nextNextLevel => true
      case Elevator(level, nextLevel +: _) if
        level < nextLevel => true
      case _ => false
    }

  val elevatorsNotGoing: Vector[Elevator] =
    elevators.filter {
      case Elevator(_, Vector()) => true
      case Elevator(level, Vector(nextLevel)) if level == nextLevel => true
      case _ => false
    }
}

object FairElevatorSystem {
  def apply(els: Vector[Elevator]) = new FairElevatorSystem(els)
  def apply(els: Vector[Elevator], d: TreeSet[Int], u: TreeSet[Int]) = new FairElevatorSystem(els, d, u)
}

