package ex2

import ex2.Direction.{North, South, West}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

class FailureRobot extends AnyFlatSpec with Matchers:
  val failureChance = 0
  val alwaysFail = 100

  "A CanFailRobot" should "turn correctly" in:
    val robot = new RobotCanFail(SimpleRobot((0, 0), Direction.North), 0)
    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "never turn" in:
    val robot =new RobotCanFail(SimpleRobot((0, 0), Direction.North), alwaysFail)
    robot.turn(Direction.East)
    robot.direction should be(Direction.North)

    robot.turn(Direction.South)
    robot.direction should be(Direction.North)

    robot.turn(Direction.West)
    robot.direction should be(Direction.North)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new RobotCanFail(SimpleRobot((0, 0), Direction.North), 0)
    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  it should "never act" in:
    val robot =new RobotCanFail(SimpleRobot((0, 0), Direction.North), alwaysFail)
    robot.act()
    robot.position should be((0, 0))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((0, 0))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((0, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))
