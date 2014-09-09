package simulations

import math.random

case class Room(val row: Int, val col: Int)

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18
    val moveInTime = 5
  }

  import SimConfig._

  val totalInfected = population * prevalenceRate
  val persons: List[Person] = List.tabulate(population - 1) { id =>
    val p = new Person(id + 1)
    if (id + 1 <= totalInfected) p.infect
    p.move
    p
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    private var actions: List[Action] = List()

    def reset() {
      infected = false
      sick = false
      immune = false
      dead = false
    }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }

    def infect {
      def infectAction(): Unit = {
        infected = true
        afterDelay(incubationTime) {
          sick = true
        }
        afterDelay(dieTime) {
          if (random.toDouble <= dieRate) dead = true
        }
        afterDelay(immuneTime) {
          if (!dead) {
            immune = true; sick = false
          }
        }
        afterDelay(healTime) {
          if (!dead) reset
        }
      }
      addAction(infectAction)
    }

    def move {
      def moveAction(): Unit = {
        val moveInDay = randomBelow(moveInTime) + 1
        afterDelay(moveInDay) {
          if (!dead) {
            val randomNeighbor = uninfectedRoom(neighborRooms(Room(row, col)))
            randomNeighbor match {
              case Some(r) => moveTo(r)
              case None =>
            }
            addAction(moveAction)
          }
        }
      }
      addAction(moveAction)
    }

    def contaminated = sick | dead
    def presentInRoom(room: Room) = room.row == row && room.col == col
    def moveTo(room: Room) = {
      row = room.row
      col = room.col
      if (!infected && random.toDouble <= transRate) infect
    }
  }

  def contaminatedRoom(room: Room): Boolean = {
    persons.exists(p => p.presentInRoom(room) && p.contaminated)
  }

  def incubationRoom(room: Room): Boolean = {
    persons.exists(p => p.presentInRoom(room) && p.infected)
  }

  def neighborRooms(room: Room): List[Room] = {
    val left = (room.col - 1 + roomRows) % roomRows
    val righ = (room.col + 1) % roomRows
    val top = (room.row + 1 + roomColumns) % roomColumns
    val bootom = (room.row + 1) % roomRows
    List(left, righ).map(col => new Room(room.row, col)) ::: List(top, bootom).map(row => new Room(row, room.col))
  }

  def uninfectedRoom(rooms: List[Room]): Option[Room] = rooms match {
    case Nil => None
    case rs => {
      val idx = (random * rs.length).toInt
      if (!contaminatedRoom(rs(idx))) Some(rs(idx))
      else uninfectedRoom(rooms diff List(rs(idx)))
    }
  }

}