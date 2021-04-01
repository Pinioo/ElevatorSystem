# Elevator System
This repository contains two implementations of systems 
handling multiple elevators calls and a simple CLI simulating
a behavior of elevators (possible calling from floor/elevator, single stepping,
printing state of the system).

### Running simulation
Project is sbt-based. Only sbt and JDK are needed to run this program.

Steps to run:
- run sbt in root directory of this project
- in sbt
  - run - run the simulation
  - test - run all tests

### Simulation usage
- c (floor) (direction) - call from floor to direction 
  (u - Up, d - Down)
- e (elevatorNumber) (floor) - call from inside the 
  elevator with index elevatorNumber
- s - make a single step
- p - print the state of the system
- sp - step and print
- q - quit

## Implementations
### SimpleElevatorSystem
Every call is put in the elevator's FIFO queue.
Elevator for call from floor choice is round-robin
### FairElevatorSystem
Calling from a floor:

System looks for the closest elevator (of those
going towards this floor or unoccupied)
to handle the call. If none found, call is queued
in suitable set

Calling from an elevator:

If a call is about a floor in the direction of
elevator's movement it is put in a sorted vector
of its goals. Otherwise, it is put in the queue
of elevator's calls