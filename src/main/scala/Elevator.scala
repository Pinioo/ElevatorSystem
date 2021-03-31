package org.pinowski.elevatorsystem

import scala.collection.immutable.Queue

case class Elevator(
                     level: Int,
                     goals: Vector[Int]
                   ) {
  def step(): Elevator = {
    (direction, goals) match {
      case (_, this.level +: gs) => Elevator(level, gs)
      case (Some(ElevatorUp), _) => Elevator(level + 1, goals)
      case (Some(ElevatorDown), _) => Elevator(level - 1, goals)
      case _ => this
    }
  }

  def direction: Option[ElevatorDirection] = {
    val directionSign = goals match {
      case this.level +: g +: _ => (g - level).sign
      case g +: _ => (g - level).sign
      case _ => 0
    }
    directionSign match {
      case -1 => Option(ElevatorDown)
      case 1 => Option(ElevatorUp)
      case _ => None
    }
  }

  def updatedGoals(newGoals: Vector[Int]): Elevator = Elevator(level, newGoals)
}
