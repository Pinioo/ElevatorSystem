package org.pinowski.elevatorsystem

import scala.collection.immutable.Queue

case class Elevator(
                     level: Int,
                     goals: Queue[Int] = Queue.empty
                   ) {
  def step(): Elevator = {
    goals match {
      case this.level +: gs => Elevator(level, gs)
      case g +: _ =>           Elevator(level + (g - level).sign, goals)
      case _ =>                this
    }
  }

  def updatedGoals(newGoals: Queue[Int]): Elevator = Elevator(level, newGoals)
}
