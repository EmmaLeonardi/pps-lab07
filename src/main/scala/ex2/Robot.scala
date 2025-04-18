package ex2

import ex2.Direction.North

import scala.language.postfixOps
import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, val batteryTurn: Int = 5, val batteryAct: Int = 10) extends Robot:
  export robot.{position, direction}
  private var battery = 100
  require(batteryTurn>=0)
  require(batteryAct>=0)

  def recharge(): Unit = battery = 100

  def batteryLevel: Int = battery

  override def act(): Unit =
    if battery - batteryAct >= 0
    then
      battery = battery - batteryAct
      robot.act()

  override def turn(dir: Direction): Unit =
    if battery - batteryTurn >= 0
    then
      battery = battery - batteryTurn
      robot.turn(dir)

  override def toString: String = s"${robot.toString} with battery level: ${battery}"

class RobotCanFail(val robot: Robot, val failureProbability: Int = 50) extends Robot:
  export robot.{position, direction}
  private val random = Random()
  require(failureProbability>=0&&failureProbability<=100)

  override def act(): Unit =
    if random.nextInt(100) > failureProbability
    then robot.act()

  override def turn(dir: Direction): Unit =
    if random.nextInt(100) > failureProbability
    then robot.turn(dir)

  override def toString: String = s"${robot.toString} with chance to fail: ${failureProbability}%"

class RobotRepeated(val robot: Robot, val numberOfRepetitions: Int = 2) extends Robot:
    export robot.{position, direction}
    require(numberOfRepetitions>=1)
    override def act(): Unit =
      for (i<- 1 to numberOfRepetitions) robot.act()

    override def turn(dir: Direction): Unit =
      for (i<- 1 to numberOfRepetitions) robot.turn(dir)

    override def toString: String = s"${robot.toString} with ${numberOfRepetitions} repetions"

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

  val batteryRobot = RobotWithBattery(SimpleRobot((0, 0), North))
  println(batteryRobot)
  batteryRobot.act()
  println(batteryRobot)
  batteryRobot.turn(robot.direction.turnRight)
  println(batteryRobot)
  batteryRobot.recharge()
  println(batteryRobot)

  val failRobot = RobotCanFail(SimpleRobot((0,0), North), 50)
  println(failRobot)

  val repeatedRobot = RobotRepeated(SimpleRobot((0,0), North), 2)
  println(repeatedRobot)

