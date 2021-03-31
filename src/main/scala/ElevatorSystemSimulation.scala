package org.pinowski.elevatorsystem

import scala.io.StdIn

case object ElevatorSystemSimulation extends App{
  var system: ElevatorSystem = FairElevatorSystem(
    Vector.fill(5)(Elevator(0, Vector.empty))
  )
  Iterator
    .continually(StdIn.readLine())
    .takeWhile(_ != "q")
    .foreach{
      input =>
        system = input.trim.toLowerCase match {
          case s"c $level $direction" =>     // Call From Level
            callFromFloorInputParser(level, direction)
              .map{
                case (l, d) => system.callFromLevel(l, d)
              }.getOrElse{
                println("Incorrect Input")
                system
              }

          case s"e $elevatorIndex $level" => // Call From Elevator
            callFromElevatorInputParser(elevatorIndex, level)
              .map{
                case (elInd, l) => system.callFromElevator(elInd, l)
              }.getOrElse{
                println("Incorrect Input")
                system
              }

          case "s" => system.step()
          case "" => system
          case _ =>
            println("Incorrect Input")
            system
        }
        println(system.elevators.map(_.level).mkString(" | "))

    }

  def callFromFloorInputParser(levelStr: String, directionStr: String): Option[(Int, ElevatorDirection)] = {
    for (
      l <- levelStr.toIntOption;
      d <- ElevatorDirection(directionStr)
    ) yield (l, d)
  }

  def callFromElevatorInputParser(elevatorStr: String, levelStr: String): Option[(Int, Int)] = {
    for (
      elInd <- elevatorStr.toIntOption;
      l     <- levelStr.toIntOption
    ) yield (elInd, l)
  }
}

