package org.pinowski.elevatorsystem

import scala.collection.View.Empty
import scala.collection.immutable.TreeSet

class FairElevatorSystem(
                          els: Vector[Elevator],
                          elevatorCalls: Vector[Option[TreeSet[Int]]],
                          downCalls: TreeSet[Int] = TreeSet.empty(Ordering.Int.reverse),
                          upCalls: TreeSet[Int] = TreeSet.empty,
                        ) extends ElevatorSystem(els) {
  def elevatorOrdering(level: Int): Ordering[(Elevator, Int)] = Ordering.by{
    case (el, _) => (el.level - level).abs
  }

  override def callFromLevel(level: Int, direction: ElevatorDirection): ElevatorSystem = {
    val elevatorsInDirection = direction match {
      case ElevatorDown =>
        elevatorsGoingDown.filter { case (el, _) => el.level >= level }
      case ElevatorUp =>
        elevatorsGoingUp.filter { case (el, _) => el.level <= level }
    }

    val elevatorWithIndexOption = (elevatorsInDirection ++ elevatorsNotGoing)
      .minOption(elevatorOrdering(level))

    elevatorWithIndexOption match {
      case Some((elevator, index)) =>
        val newGoals = direction match {
          case ElevatorDown =>
            val levelsAbove = elevator
              .goals
              .takeWhile(_ > level)
            val levelsBelow = elevator
              .goals
              .dropWhile(_ >= level)
            levelsAbove ++ Vector(level) ++ levelsBelow
          case ElevatorUp =>
            val levelsBelow = elevator
              .goals
              .takeWhile(_ < level)
            val levelsAbove = elevator
              .goals
              .dropWhile(_ <= level)
            levelsBelow ++ Vector(level) ++ levelsAbove
        }
        this.updatedElevator(index, elevator.updatedGoals(newGoals))
      case None =>
        direction match {
          case ElevatorDown =>
            this.updatedDownCalls(downCalls + level)
          case ElevatorUp =>
            this.updatedUpCalls(upCalls + level)
        }
    }

  }

  override def callFromElevator(elevatorIndex: Int, level: Int): ElevatorSystem = {
    val elevator = elevators(elevatorIndex)
    elevator.direction match {
      case None =>
        val newGoals = elevator.goals :+ level
        this.updatedElevator(elevatorIndex, elevator.updatedGoals(newGoals))

      case Some(ElevatorUp) if (level >= elevator.level) =>
        val newGoals = elevator.goals.takeWhile(_ < level) ++ Vector(level) ++ elevator.goals.dropWhile(_ <= level)
        this.updatedElevator(elevatorIndex, elevator.updatedGoals(newGoals))

      case Some(ElevatorUp) if (level < elevator.level) =>
        val currentCallsOption = elevatorCalls(elevatorIndex)
        val newCallsOption = currentCallsOption match {
          case Some(calls) => Option(calls + level)
          case None => Option(TreeSet(level)(Ordering.Int.reverse))
        }
        this.updatedElevatorCall(elevatorIndex, newCallsOption)

      case Some(ElevatorDown) if (level <= elevator.level) =>
        val newGoals = elevator.goals.takeWhile(_ > level) ++ Vector(level) ++ elevator.goals.dropWhile(_ >= level)
        this.updatedElevator(elevatorIndex, elevator.updatedGoals(newGoals))

      case Some(ElevatorDown) if (level > elevator.level) =>
        val currentCallsOption = elevatorCalls(elevatorIndex)
        val newCallsOption = currentCallsOption match {
          case Some(calls) => Option(calls + level)
          case None => Option(TreeSet(level))
        }
        this.updatedElevatorCall(elevatorIndex, newCallsOption)
    }
  }

  override def step(): ElevatorSystem = {
    FairElevatorSystem(elevators.map(_.step()), elevatorCalls, downCalls, upCalls)
      .handleElevatorCalls().asInstanceOf[FairElevatorSystem]
      .handleDownCalls.asInstanceOf[FairElevatorSystem]
      .handleUpCalls
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
      case (els, elsCalls) => this
        .updatedElevators(els)
        .updatedElevatorCalls(elsCalls)
    }
  }

  private def handleUpCalls: ElevatorSystem =
    upCalls.toSeq match {
      case firstLevel +: _ =>
        val newElevatorWithIndexOption = elevatorsNotGoing
          .minOption(elevatorOrdering(firstLevel))
          .map{
            case (el, index) => (el.updatedGoals(upCalls.toVector), index)
          }
        newElevatorWithIndexOption match {
          case Some((newElevator, index)) =>
            this
              .updatedElevator(index, newElevator)
              .updatedUpCalls(TreeSet.empty[Int])
          case None => this
        }
      case _ => this
    }

  private def handleDownCalls: ElevatorSystem =
    downCalls.toSeq match {
      case firstLevel +: _ =>
        val newElevatorWithIndexOption = elevatorsNotGoing
          .minOption(elevatorOrdering(firstLevel))
          .map {
            case (el, index) => (el.updatedGoals(downCalls.toVector), index)
          }
        newElevatorWithIndexOption match {
          case Some((newElevator, index)) => this
            .updatedElevator(index, newElevator)
            .updatedDownCalls(TreeSet.empty(Ordering.Int.reverse))
          case None => this
        }
      case _ => this
    }

  def updatedElevators(newElevators: Vector[Elevator]): FairElevatorSystem =
    FairElevatorSystem(newElevators, elevatorCalls, downCalls, upCalls)

  def updatedElevator(elevatorIndex: Int, newElevator: Elevator): FairElevatorSystem =
    this.updatedElevators(elevators.updated(elevatorIndex, newElevator))

  def updatedElevatorCalls(newElevatorCalls: Vector[Option[TreeSet[Int]]]): FairElevatorSystem =
    FairElevatorSystem(elevators, newElevatorCalls, downCalls, upCalls)

  def updatedElevatorCall(elevatorCallsIndex: Int, newElevatorCalls: Option[TreeSet[Int]]): FairElevatorSystem =
    this.updatedElevatorCalls(elevatorCalls.updated(elevatorCallsIndex, newElevatorCalls))

  def updatedDownCalls(newDownCalls: TreeSet[Int]): FairElevatorSystem =
    FairElevatorSystem(elevators, elevatorCalls, newDownCalls, upCalls)

  def updatedUpCalls(newUpCalls: TreeSet[Int]): FairElevatorSystem =
    FairElevatorSystem(elevators, elevatorCalls, downCalls, newUpCalls)

  val elevatorsGoingDown: Vector[(Elevator, Int)] =
    elevators.zipWithIndex.filter{
      case (el, _) => el.direction match {
        case Some(ElevatorDown) => true
        case _ => false
      }
    }

  val elevatorsGoingUp: Vector[(Elevator, Int)] =
    elevators.zipWithIndex.filter{
      case (el, _) => el.direction match {
        case Some(ElevatorUp) => true
        case _ => false
      }
    }

  val elevatorsNotGoing: Vector[(Elevator, Int)] =
    elevators.zipWithIndex.filter{
      case (el, _) => el.direction match {
        case None => true
        case _ => false
      }
    }

  def elevatorCalls(): Vector[Option[TreeSet[Int]]] = elevatorCalls
  def upCalls(): TreeSet[Int] = upCalls
  def downCalls(): TreeSet[Int] = downCalls
}

object FairElevatorSystem {
  def apply(els: Vector[Elevator]) = new FairElevatorSystem(els, Vector.fill(els.size)(None))

  def apply(els: Vector[Elevator], e: Vector[Option[TreeSet[Int]]], d: TreeSet[Int], u: TreeSet[Int]) = new FairElevatorSystem(els, e, d, u)

  def apply(els: Vector[Elevator], e: Vector[Option[TreeSet[Int]]]) = new FairElevatorSystem(els, e)
}

