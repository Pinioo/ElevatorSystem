package org.pinowski.elevatorsystem

import scala.io.StdIn

/**
 * Simulation for the elevator system with I/O operations
 *
 * Commands:
 *
 * c (floor) {u|d} - calling an elevator from the floor in direction Up/Down
 *
 * e (elevatorIndex) (floor) - pressing a button in elevator with
 * index elevatorIndex to go to floor
 *
 * s - make a single step of simulation
 *
 * p - print state of the system
 *
 * sp - step and print
 *
 * q - quit the program
 */
case object ElevatorSystemSimulation extends App {
  val elevatorsNumber = 3
  var system: ElevatorSystem = FairElevatorSystem(
    Vector.fill(elevatorsNumber)(Elevator(0, Vector.empty))
  )
  Iterator
    .continually(StdIn.readLine())
    .takeWhile(_ != "q")
    .foreach {
      input =>
        system = input.trim.toLowerCase match {
          case s"c $floor $direction" => // Call From floor
            callFromFloorInputParser(floor, direction)
              .map {
                case (l, d) => system.callFromFloor(l, d)
              }.getOrElse {
              println("Incorrect Input")
              system
            }

          case s"e $elevatorIndex $floor" => // Call From Elevator
            callFromElevatorInputParser(elevatorIndex, floor)
              .map {
                case (elInd, l) => system.callFromElevator(elInd, l)
              }.getOrElse {
              println("Incorrect Input")
              system
            }

          case "s" => system.step()
          case "p" =>
            println(system)
            system
          case "sp" =>
            val newSystem = system.step()
            println(newSystem)
            newSystem
          case "" => system
          case _ =>
            println("Incorrect Input")
            system
        }
    }

  def callFromFloorInputParser(floorStr: String, directionStr: String): Option[(Int, ElevatorDirection)] = {
    for (
      l <- floorStr.toIntOption;
      d <- ElevatorDirection(directionStr)
    ) yield (l, d)
  }

  def callFromElevatorInputParser(elevatorStr: String, floorStr: String): Option[(Int, Int)] = {
    for (
      elInd <- elevatorStr.toIntOption if system.elevators.indices.contains(elInd);
      l <- floorStr.toIntOption
    ) yield (elInd, l)
  }
}
