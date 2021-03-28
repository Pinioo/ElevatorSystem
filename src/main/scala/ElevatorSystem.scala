package org.pinowski.elevatorsystem

abstract case class ElevatorSystem(elevators: Vector[Elevator]) {
  /**
   * Function to be used when someone calls
   * an elevator from certain level
   * @param level from which a person is calling an elevator
   * @param direction in which a calling person is willing to go
   * @return new state of an elevator system
   */
  def callFromLevel(level: Int, direction: ElevatorDirection): ElevatorSystem

  /**
   * Function to be called if someone in elevator
   * clicked a button in order to go to some level
   * @param elevatorIndex index of the elevator in elevators array
   * @param level which a person is willing to be taken to
   * @return new state of an elevator system
   */
  def callFromElevator(elevatorIndex: Int, level: Int) : ElevatorSystem

  /**
   * Function which gives state of elevator system
   * after single step
   * @return new state of an elevator system
   */
  def step(): ElevatorSystem
}