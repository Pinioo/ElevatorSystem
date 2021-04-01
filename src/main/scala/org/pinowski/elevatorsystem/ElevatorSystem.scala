package org.pinowski.elevatorsystem

abstract case class ElevatorSystem(elevators: Vector[Elevator]) {
  /**
   * Function to be used when someone calls
   * an elevator from certain floor
   *
   * @param floor     from which a person is calling an elevator
   * @param direction in which a calling person is willing to go
   * @return new state of an elevator system
   */
  def callFromFloor(floor: Int, direction: ElevatorDirection): ElevatorSystem

  /**
   * Function to be called if someone in elevator
   * clicked a button in order to go to some floor
   *
   * @param elevatorIndex index of the elevator in elevators array
   * @param floor         which a person is willing to be taken to
   * @return new state of an elevator system
   */
  def callFromElevator(elevatorIndex: Int, floor: Int): ElevatorSystem

  /**
   * Function which gives state of elevator system
   * after single step
   *
   * @return new state of an elevator system
   */
  def step(): ElevatorSystem

  override def toString: String = elevators.mkString(" | ")
}
