package org.pinowski.elevatorsystem

import scala.collection.immutable.TreeSet

class FairElevatorSystem(
                          els: Vector[Elevator],
                          elevatorCalls: Vector[Option[TreeSet[Int]]],
                          downCalls: TreeSet[Int] = TreeSet.empty(Ordering.Int.reverse),
                          upCalls: TreeSet[Int] = TreeSet.empty,
                        ) extends ElevatorSystem(els) {
  override def callFromLevel(level: Int, direction: ElevatorDirection): ElevatorSystem = {
    val elevatorOption: Option[Elevator] = direction match {
      case ElevatorDown =>
        (elevatorsGoingDown.filter(el => el.level >= level) ++ elevatorsNotGoing)
          .minOption(Ordering.by[Elevator, Int](el => (el.level - level).abs))
      case ElevatorUp =>
        (elevatorsGoingUp.filter(el => el.level <= level) ++ elevatorsNotGoing)
          .minOption(Ordering.by[Elevator, Int](el => (el.level - level).abs))
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
        FairElevatorSystem(elevators.updated(elevatorIndex, elevator.updatedGoals(newGoals)), elevatorCalls, downCalls, upCalls)
      case None =>
        direction match {
          case ElevatorDown =>
            FairElevatorSystem(elevators, elevatorCalls, downCalls + level, upCalls)
          case ElevatorUp =>
            FairElevatorSystem(elevators, elevatorCalls, downCalls, upCalls + level)
        }
    }

  }

  override def callFromElevator(elevatorIndex: Int, level: Int): ElevatorSystem = {
    val elevator = elevators(elevatorIndex)
    elevator.direction match {
      case None =>
        val newGoals = elevator.goals :+ level
        FairElevatorSystem(elevators.updated(elevatorIndex, elevator.updatedGoals(newGoals)), elevatorCalls, downCalls, upCalls)

      case Some(ElevatorUp) if (level >= elevator.level) =>
        val newGoals = elevator.goals.takeWhile(_ < level) ++ Vector(level) ++ elevator.goals.dropWhile(_ <= level)
        FairElevatorSystem(elevators.updated(elevatorIndex, elevator.updatedGoals(newGoals)), elevatorCalls, downCalls, upCalls)

      case Some(ElevatorUp) if (level < elevator.level) =>
        val currentCallsOption = elevatorCalls(elevatorIndex)
        val newCallsOption = currentCallsOption match {
          case Some(calls) => Option(calls + level)
          case None => Option(TreeSet(level)(Ordering.Int.reverse))
        }
        FairElevatorSystem(elevators, elevatorCalls.updated(elevatorIndex, newCallsOption), downCalls, upCalls)

      case Some(ElevatorDown) if (level <= elevator.level) =>
        val newGoals = elevator.goals.takeWhile(_ > level) ++ Vector(level) ++ elevator.goals.dropWhile(_ >= level)
        FairElevatorSystem(elevators.updated(elevatorIndex, elevator.updatedGoals(newGoals)), elevatorCalls, downCalls, upCalls)

      case Some(ElevatorDown) if (level > elevator.level) =>
        val currentCallsOption = elevatorCalls(elevatorIndex)
        val newCallsOption = currentCallsOption match {
          case Some(calls) => Option(calls + level)
          case None => Option(TreeSet(level))
        }
        FairElevatorSystem(elevators, elevatorCalls.updated(elevatorIndex, newCallsOption), downCalls, upCalls)
    }
  }

  override def step(): ElevatorSystem = {
    FairElevatorSystem(elevators.map(_.step()), elevatorCalls, downCalls, upCalls)
      .handleElevatorCalls().asInstanceOf[FairElevatorSystem]
      .handleDownCalls().asInstanceOf[FairElevatorSystem]
      .handleUpCalls()
  }

  private def handleElevatorCalls(): ElevatorSystem = {
    val newElevatorsAndCalls = elevators.zip(elevatorCalls).map{
      case (elevator, callsOption) =>
        (elevator.direction, callsOption) match {
          case (None, Some(calls)) => (elevator.updatedGoals(calls.toVector), None)
          case _ => (elevator, callsOption)
        }
    }
    newElevatorsAndCalls.unzip match {
      case (els, elsCalls) => FairElevatorSystem(els, elsCalls, downCalls, upCalls)
    }
  }

  private def handleUpCalls(): ElevatorSystem = {
    this
  }

  private def handleDownCalls(): ElevatorSystem = {
    this
  }

  val elevatorsGoingDown: Vector[Elevator] =
    elevators.filter{
      _.direction match {
        case Some(ElevatorDown) => true
        case _ => false
      }
    }

  val elevatorsGoingUp: Vector[Elevator] =
    elevators.filter{
      _.direction match {
        case Some(ElevatorUp) => true
        case _ => false
      }
    }

  val elevatorsNotGoing: Vector[Elevator] =
    elevators.filter{
      _.direction match {
        case None => true
        case _ => false
      }
    }

  def elevatorCalls(): Vector[Option[TreeSet[Int]]] = elevatorCalls
}

object FairElevatorSystem {
  def apply(els: Vector[Elevator]) = new FairElevatorSystem(els, Vector.fill(els.size)(None))

  def apply(els: Vector[Elevator], e: Vector[Option[TreeSet[Int]]], d: TreeSet[Int], u: TreeSet[Int]) = new FairElevatorSystem(els, e, d, u)

  def apply(els: Vector[Elevator], e: Vector[Option[TreeSet[Int]]]) = new FairElevatorSystem(els, e)
}

