package org.pinowski.elevatorsystem

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElevatorTest extends AnyFlatSpec with Matchers {
  val testElevatorDown = Elevator(10, Vector(8))
  val testElevatorDownAfter = Elevator(9, Vector(8))
  val testElevatorUp = Elevator(8, Vector(10))
  val testElevatorUpAfter = Elevator(9, Vector(10))
  val testElevatorStays1 = Elevator(8, Vector(8))
  val testElevatorStays1After = Elevator(8, Vector())
  val testElevatorStays2 = Elevator(8, Vector())
  val testElevatorStays2After = Elevator(8, Vector())

  "Elevator" should "make a step, go in direction of next floor or delete destination if reached" in {
    testElevatorDown.step() shouldEqual testElevatorDownAfter
    testElevatorUp.step() shouldEqual testElevatorUpAfter
    testElevatorStays1.step() shouldEqual testElevatorStays1After
    testElevatorStays2.step() shouldEqual testElevatorStays2After
  }
}
