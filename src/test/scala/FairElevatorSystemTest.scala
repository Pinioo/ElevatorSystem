package org.pinowski.elevatorsystem

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class FairElevatorSystemTest extends AnyFlatSpec with matchers.should.Matchers {
  val elevatorDownFamily = Vector(
    Elevator(10, Vector(8)),
    Elevator(10, Vector(8, 10)),
    Elevator(10, Vector(8, 6)),
    Elevator(10, Vector(10, 8))
  )

  val elevatorUpFamily = Vector(
    Elevator(8, Vector(10)),
    Elevator(8, Vector(10, 8)),
    Elevator(8, Vector(10, 12)),
    Elevator(8, Vector(8, 10))
  )

  val elevatorStaysFamily = Vector(
    Elevator(8, Vector()),
    Elevator(8, Vector(8))
  )

  val testSystem: FairElevatorSystem = FairElevatorSystem(
    elevatorDownFamily ++ elevatorUpFamily ++ elevatorStaysFamily
  )

  "FairElevatorSystem" should "recognize elevators going up" in {
    testSystem.elevatorsGoingUp should contain theSameElementsAs elevatorUpFamily
  }


  it should "recognize elevators going down" in {
    testSystem.elevatorsGoingDown should contain theSameElementsAs elevatorDownFamily
  }

  it should "recognize elevators not going" in {
    testSystem.elevatorsNotGoing should contain theSameElementsAs elevatorStaysFamily
  }

  it should "make a proper step" in {
    testSystem.step().elevators should contain theSameElementsInOrderAs Vector(
      Elevator(9, Vector(8)),
      Elevator(9, Vector(8, 10)),
      Elevator(9, Vector(8, 6)),
      Elevator(10, Vector(8)),
      Elevator(9, Vector(10)),
      Elevator(9, Vector(10, 8)),
      Elevator(9, Vector(10, 12)),
      Elevator(8, Vector(10)),
      Elevator(8, Vector()),
      Elevator(8, Vector())
    )
  }

  it should "update goals of some elevator after call from level if there is a suitable one" in {
    FairElevatorSystem(Vector(
      Elevator(12, Vector(10, 8)),
      Elevator(5, Vector(11, 12)),
      Elevator(13, Vector())
    )).callFromLevel(9, ElevatorDown)
      .callFromLevel(20, ElevatorUp)
      .callFromLevel(5, ElevatorUp)
      .elevators should contain theSameElementsInOrderAs Vector(
        Elevator(12, Vector(10, 9, 8)),
        Elevator(5, Vector(5, 11, 12)),
        Elevator(13, Vector(20))
    )
  }
}
