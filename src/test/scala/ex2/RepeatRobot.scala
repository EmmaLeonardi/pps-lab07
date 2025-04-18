package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RepeatRobot extends AnyFlatSpec with Matchers:
  "A Repeated robot" should "turn correctly" in:
    val robot = new RobotRepeated(SimpleRobot((0, 0), Direction.North), 2)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new RobotRepeated(SimpleRobot((0, 0), Direction.North), 2)

    robot.act()
    robot.position should be((0, 2))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((2, 2))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((2, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))
