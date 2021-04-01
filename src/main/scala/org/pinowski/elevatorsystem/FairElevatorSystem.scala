package org.pinowski.elevatorsystem

import scala.collection.immutable.TreeSet

/**
 * Class implementing fairer solution to elevators
 * calling problem
 *
 * Calling from floor:
 *
 * System looks for the closest elevator (of those
 * going towards this floor or unoccupied)
 * to handle the call. If none found, call is queued
 * in suitable set
 *
 * Calling from elevator:
 *
 * If a call is about a floor in the direction of
 * elevator's movement it is put in sorted vector
 * of its goals. Otherwise, it is put in the queue
 * of elevator's calls
 *
 * @param els vector of elevators state
 * @param elevatorCalls vector of optional sets with
 *                      calls from elevators which
 *                      have not been processed yet
 * @param downCalls set with down calls from floors which
 *                  have not been processed yet
 * @param upCalls set with up calls from floors which
 *                have not been processed yet
 */
class FairElevatorSystem(
                          els: Vector[Elevator],
                          elevatorCalls: Vector[Option[TreeSet[Int]]],
                          downCalls: TreeSet[Int] = TreeSet.empty(Ordering.Int.reverse),
                          upCalls: TreeSet[Int] = TreeSet.empty,
                        ) extends ElevatorSystem(els) {
  def elevatorOrdering(floor: Int): Ordering[(Elevator, Int)] = Ordering.by{
    case (el, _) => (el.floor - floor).abs
  }

  override def callFromFloor(floor: Int, direction: ElevatorDirection): ElevatorSystem = {
    val elevatorsInDirection = direction match {
      case ElevatorDown =>
        elevatorsGoingDown.filter { case (el, _) => el.floor >= floor }
      case ElevatorUp =>
        elevatorsGoingUp.filter { case (el, _) => el.floor <= floor }
    }

    val elevatorWithIndexOption = (elevatorsInDirection ++ elevatorsNotGoing)
      .minOption(elevatorOrdering(floor))

    elevatorWithIndexOption match {
      case Some((elevator, index)) =>
        val newGoals = direction match {
          case ElevatorDown =>
            val floorsAbove = elevator
              .goals
              .takeWhile(_ > floor)
            val floorsBelow = elevator
              .goals
              .dropWhile(_ >= floor)
            floorsAbove ++ Vector(floor) ++ floorsBelow
          case ElevatorUp =>
            val floorsBelow = elevator
              .goals
              .takeWhile(_ < floor)
            val floorsAbove = elevator
              .goals
              .dropWhile(_ <= floor)
            floorsBelow ++ Vector(floor) ++ floorsAbove
        }
        this.updatedElevator(
          index,
          elevator.updatedGoals(newGoals).updatedDirection(Option(direction))
        )
      case None =>
        direction match {
          case ElevatorDown =>
            this.updatedDownCalls(downCalls + floor)
          case ElevatorUp =>
            this.updatedUpCalls(upCalls + floor)
        }
    }

  }

  override def callFromElevator(elevatorIndex: Int, floor: Int): ElevatorSystem = {
    val elevator = elevators(elevatorIndex)
    elevator.getDirection match {
      case None =>
        val newGoals = elevator.goals :+ floor
        this.updatedElevator(
          elevatorIndex,
          elevator
            .updatedGoals(newGoals)
            .updatedDirection(ElevatorDirection.direction(elevator.floor, floor))
        )

      case Some(ElevatorUp) =>
        if (floor >= elevator.floor) { // elevator going towards floor
          val newGoals =
            elevator.goals.takeWhile(_ < floor) ++
            Vector(floor) ++
            elevator.goals.dropWhile(_ <= floor)
          this.updatedElevator(
            elevatorIndex,
            elevator
              .updatedGoals(newGoals)
              .updatedDirection(ElevatorDirection.direction(elevator.floor, floor))
          )
        }
        else { // elevator going in other direction
          val currentCallsOption = elevatorCalls(elevatorIndex)
          val newCallsOption = currentCallsOption match {
            case Some(calls) => Option(calls + floor)
            case None => Option(TreeSet(floor)(Ordering.Int.reverse))
          }
          this.updatedElevatorCall(elevatorIndex, newCallsOption)
        }

      case Some(ElevatorDown) =>
        if (floor <= elevator.floor) { // elevator going towards floor
          val newGoals =
            elevator.goals.takeWhile(_ > floor) ++
            Vector(floor) ++
            elevator.goals.dropWhile(_ >= floor)
          this.updatedElevator(
            elevatorIndex,
            elevator
              .updatedGoals(newGoals)
              .updatedDirection(ElevatorDirection.direction(elevator.floor, floor))
          )
        }
        else { // elevator going in other direction
          val currentCallsOption = elevatorCalls(elevatorIndex)
          val newCallsOption = currentCallsOption match {
            case Some(calls) => Option(calls + floor)
            case None => Option(TreeSet(floor))
          }
          this.updatedElevatorCall(elevatorIndex, newCallsOption)
        }
    }
  }

  /**
   * @return state of the system after single step
   */
  override def step(): ElevatorSystem = {
    FairElevatorSystem(elevators.map(_.step()), elevatorCalls, downCalls, upCalls)
      .handleStoppedElevators()
      .handleElevatorCalls()
      .handleDownCalls
      .handleUpCalls
  }

  private def handleStoppedElevators(): FairElevatorSystem = {
    val newElevators = elevators.map{
      el =>
        el.goals match {
          case Vector() => el.updatedDirection(None)
          case _ => el
        }
    }
    this.updatedElevators(newElevators)
  }

  /**
   * Function to be called by this.step() to take care of
   * queued calls from elevators which could not be processed before
   *
   * For elevators which stopped, they are given all of those
   * calls and new state of system with updated elevators is created
   *
   * @return new (or same) state of the system
   */
  private def handleElevatorCalls(): FairElevatorSystem = {
    val newElevatorsAndCalls = elevators.zip(elevatorCalls).map{
      case (elevator, callsOption) =>
        (elevator.getDirection, callsOption) match {
          case (None, Some(calls)) => (
            elevator
              .updatedGoals(calls.toVector)
              .updatedDirection{
                if (calls.head < elevator.floor) {
                  Some(ElevatorDown)
                } else {
                  Some(ElevatorUp)
                }
              },
            None
          )
          case _ => (elevator, callsOption)
        }
    }
    newElevatorsAndCalls.unzip match {
      case (els, elsCalls) => this
        .updatedElevators(els)
        .updatedElevatorCalls(elsCalls)
    }
  }

  /**
   * Function to be called by this.step() to take care of
   * queued up calls which could not be processed before
   *
   * If suitable elevator is found, it is given all of those
   * calls and new state of system is created
   *
   * @return new (or same) state of the system
   */
  private def handleUpCalls: FairElevatorSystem =
    upCalls.toSeq match {
      case firstFloor +: _ =>
        val newElevatorWithIndexOption = elevatorsNotGoing
          .minOption(elevatorOrdering(firstFloor))
          .map{
            case (el, index) => (
              el
                .updatedGoals(upCalls.toVector)
                .updatedDirection(Some(ElevatorUp)),
              index
            )
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

  /**
   * Function to be called by this.step() to take care of
   * queued down calls which could not be processed before
   *
   * If suitable elevator is found, it is given all of those
   * calls and new state of system is created
   *
   * @return new (or same) state of the system
   */
  private def handleDownCalls: FairElevatorSystem =
    downCalls.toSeq match {
      case firstFloor +: _ =>
        val newElevatorWithIndexOption = elevatorsNotGoing
          .minOption(elevatorOrdering(firstFloor))
          .map {
            case (el, index) => (
              el
                .updatedGoals(downCalls.toVector)
                .updatedDirection(Some(ElevatorDown)),
              index
            )
          }
        newElevatorWithIndexOption match {
          case Some((newElevator, index)) => this
            .updatedElevator(index, newElevator)
            .updatedDownCalls(TreeSet.empty(Ordering.Int.reverse))
          case None => this
        }
      case _ => this
    }

  /**
   * @param newElevators to be applied to this state
   * @return this state with new elevators vector
   */
  def updatedElevators(newElevators: Vector[Elevator]): FairElevatorSystem =
    FairElevatorSystem(newElevators, elevatorCalls, downCalls, upCalls)

  /**
   * @param elevatorIndex index of elevator to be updated
   * @param newElevator state to be applied
   * @return this state with one elevator changed
   */
  def updatedElevator(elevatorIndex: Int, newElevator: Elevator): FairElevatorSystem =
    this.updatedElevators(elevators.updated(elevatorIndex, newElevator))

  /**
   * @param newElevatorCalls to be applied to this state
   * @return this state with new elevator calls vector
   */
  def updatedElevatorCalls(newElevatorCalls: Vector[Option[TreeSet[Int]]]): FairElevatorSystem =
    FairElevatorSystem(elevators, newElevatorCalls, downCalls, upCalls)

  /**
   * @param elevatorCallsIndex index of calls to be updated
   * @param newElevatorCalls set to be applied
   * @return this state with one elevator calls set changed
   */
  def updatedElevatorCall(elevatorCallsIndex: Int, newElevatorCalls: Option[TreeSet[Int]]): FairElevatorSystem =
    this.updatedElevatorCalls(elevatorCalls.updated(elevatorCallsIndex, newElevatorCalls))

  /**
   * @param newDownCalls to be applied to this state
   * @return this state with new down calls set
   */
  def updatedDownCalls(newDownCalls: TreeSet[Int]): FairElevatorSystem =
    FairElevatorSystem(elevators, elevatorCalls, newDownCalls, upCalls)

  /**
   * @param newUpCalls to be applied to this state
   * @return this state with new up calls set
   */
  def updatedUpCalls(newUpCalls: TreeSet[Int]): FairElevatorSystem =
    FairElevatorSystem(elevators, elevatorCalls, downCalls, newUpCalls)


  /**
   * @return Vector of (elevator, index) for elevators going down
   */
  def elevatorsGoingDown: Vector[(Elevator, Int)] =
    elevators.zipWithIndex.filter{
      case (el, _) => el.getDirection match {
        case Some(ElevatorDown) => true
        case _ => false
      }
    }

  /**
   * @return Vector of (elevator, index) for elevators going up
   */
  def elevatorsGoingUp: Vector[(Elevator, Int)] =
    elevators.zipWithIndex.filter{
      case (el, _) => el.getDirection match {
        case Some(ElevatorUp) => true
        case _ => false
      }
    }

  /**
   * @return Vector of (elevator, index) for waiting elevators
   */
  def elevatorsNotGoing: Vector[(Elevator, Int)] =
    elevators.zipWithIndex.filter{
      case (el, _) => el.getDirection match {
        case None => true
        case _ => false
      }
    }

  def elevatorCalls(): Vector[Option[TreeSet[Int]]] = elevatorCalls
  def upCalls(): TreeSet[Int] = upCalls
  def downCalls(): TreeSet[Int] = downCalls

  override def toString: String = {
    "Down Calls Queue: " +
    downCalls.mkString("(", ", ", ")\n") +
    "Up Calls Queue: " +
    upCalls.mkString("(", ", ", ")\n") +
    elevators.zip(elevatorCalls).zipWithIndex.map {
      case ((el, call), index) =>
        Seq(
          s"$index. :",
          s"Floor: ${el.floor}",
          s"Goals: ${el.goals.mkString(" -> ")}",
          s"Direction: ${el.getDirection match {
            case Some(ElevatorUp) => "Up"
            case Some(ElevatorDown) => "Down"
            case None => "None"
          }}",
          s"Inside Calls Queue: ${call.mkString("(",", ",")")}"
        ).mkString("\n\t")
    }.mkString("\n\n")
  }
}

object FairElevatorSystem {
  def apply(els: Vector[Elevator]) =
    new FairElevatorSystem(
      els.map(el => el.updatedDirection(el.getDirection)),
      Vector.fill(els.size)(None)
    )

  def apply(els: Vector[Elevator], e: Vector[Option[TreeSet[Int]]], d: TreeSet[Int], u: TreeSet[Int]) =
    new FairElevatorSystem(
      els.map(el => el.updatedDirection(el.getDirection)),
      e, d, u
    )

  def apply(els: Vector[Elevator], e: Vector[Option[TreeSet[Int]]]) =
    new FairElevatorSystem(
      els.map(el => el.updatedDirection(el.getDirection)),
      e
    )
}

