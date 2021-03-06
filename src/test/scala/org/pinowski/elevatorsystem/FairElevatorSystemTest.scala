package org.pinowski.elevatorsystem

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.TreeSet

class FairElevatorSystemTest extends AnyFlatSpec with matchers.should.Matchers {
  val elevatorDownFamily: Vector[Elevator] = Vector(
    Elevator(10, Vector(8)),
    Elevator(10, Vector(8, 6))
  ).map(el => el.updatedDirection(el.getDirection))

  val elevatorUpFamily: Vector[Elevator] = Vector(
    Elevator(8, Vector(10)),
    Elevator(8, Vector(10, 12))
  ).map(el => el.updatedDirection(el.getDirection))

  val elevatorStaysFamily: Vector[Elevator] = Vector(
    Elevator(8, Vector()),
    Elevator(8, Vector(8))
  ).map(el => el.updatedDirection(el.getDirection))

  val testSystem: FairElevatorSystem = FairElevatorSystem(
    elevatorDownFamily ++ elevatorUpFamily ++ elevatorStaysFamily
  )

  "FairElevatorSystem" should "recognize elevators going up" in {
    testSystem
      .elevatorsGoingUp
      .map(_._1) should contain theSameElementsAs elevatorUpFamily
  }


  it should "recognize elevators going down" in {
    testSystem
      .elevatorsGoingDown
      .map(_._1) should contain theSameElementsAs elevatorDownFamily
  }

  it should "recognize elevators not going" in {
    testSystem
      .elevatorsNotGoing
      .map(_._1) should contain theSameElementsAs elevatorStaysFamily
  }

  it should "make a proper step" in {
    testSystem.step().elevators should contain theSameElementsInOrderAs Vector(
      Elevator(9, Vector(8), Some(Some(ElevatorDown))),
      Elevator(9, Vector(8, 6), Some(Some(ElevatorDown))),
      Elevator(9, Vector(10), Some(Some(ElevatorUp))),
      Elevator(9, Vector(10, 12), Some(Some(ElevatorUp))),
      Elevator(8, Vector(), Some(None)),
      Elevator(8, Vector(), Some(None))
    )
  }

  it should "update goals of some elevator after call from floor if there is a suitable one" in {
    FairElevatorSystem(Vector(
      Elevator(12, Vector(10, 8)),
      Elevator(5, Vector(11, 12)),
      Elevator(13, Vector())
    )).callFromFloor(9, ElevatorDown)
      .callFromFloor(20, ElevatorUp)
      .callFromFloor(5, ElevatorUp)
      .elevators
      .map(_.goals) should contain theSameElementsInOrderAs Vector(
        Vector(10, 9, 8),
        Vector(5, 11, 12),
        Vector(20)
    )
  }

  it should "apply calls from elevator if specific elevator is going this way" in {
    FairElevatorSystem(Vector(
      Elevator(12, Vector(10, 8)),
      Elevator(5, Vector(11, 12)),
      Elevator(13, Vector())
    )).callFromElevator(1, 8)
      .elevators
      .map(_.goals) should contain theSameElementsInOrderAs Vector(
      Vector(10, 8),
      Vector(8, 11, 12),
      Vector()
    )
  }

  it should "queue calls from elevator if specific elevator is not going this way" in {
    val system = FairElevatorSystem(Vector(
      Elevator(12, Vector(10, 8)),
      Elevator(5, Vector(11, 12)),
      Elevator(13, Vector())
    )).callFromElevator(1, 2)
      .callFromElevator(1, 4)
      .callFromElevator(0, 18)
      .callFromElevator(0, 14)
      .asInstanceOf[FairElevatorSystem]

    system.elevators.map(_.goals) should contain theSameElementsInOrderAs Vector(
      Vector(10, 8),
      Vector(11, 12),
      Vector()
    )

    system.elevatorCalls().map(_.map(_.toSeq)) should contain theSameElementsInOrderAs Vector(
      Option(Seq(14, 18)),
      Option(Seq(4, 2)),
      None
    )
  }

  it should "apply elevator calls from queue and clear queue if elevator stopped" in {
    val system = FairElevatorSystem(
      Vector(
        Elevator(13, Vector(13), Some(Some(ElevatorUp)))
      ),
      Vector(
        Option(TreeSet(10, 8)(Ordering.Int.reverse))
      )
    ).step()
      .asInstanceOf[FairElevatorSystem]

    system.elevators should contain theSameElementsInOrderAs Vector(
      Elevator(13, Vector(10, 8), Some(Some(ElevatorDown)))
    )

    system.elevatorCalls should contain theSameElementsInOrderAs Vector(
      None
    )
  }

  it should "apply queued up and down calls if some elevators stopped and clear queues" in {
    val system = FairElevatorSystem(
      Vector(
        Elevator(5, Vector()),
        Elevator(13, Vector())
      ),
      Vector(
        None,
        None
      ),
      TreeSet(8, 3, 2)(Ordering.Int.reverse),
      TreeSet(4, 10)
    ).step()
      .asInstanceOf[FairElevatorSystem]

    system.elevators should contain theSameElementsInOrderAs Vector(
      Elevator(5, Vector(8, 3, 2), Some(Some(ElevatorDown))),
      Elevator(13, Vector(4, 10), Some(Some(ElevatorUp)))
    )

    system.upCalls shouldBe empty
    system.downCalls shouldBe empty
  }

}
