package ex2

import ex2.Direction.{North, South, West}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

class BatteryRobot extends AnyFlatSpec with Matchers:
  var consumptionAction = 1

  "A BatteryRobot" should "turn correctly" in:
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      consumptionAction,
      consumptionAction
    )
    var originalBattery = robot.batteryLevel
    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    robot.batteryLevel should be(originalBattery - consumptionAction)

    originalBattery = robot.batteryLevel
    robot.turn(Direction.South)
    robot.direction should be(Direction.South)
    robot.batteryLevel should be(originalBattery - consumptionAction)

    originalBattery = robot.batteryLevel
    robot.turn(Direction.West)
    robot.direction should be(Direction.West)
    robot.batteryLevel should be(originalBattery - consumptionAction)

    originalBattery = robot.batteryLevel
    robot.turn(Direction.North)
    robot.direction should be(Direction.North)
    robot.batteryLevel should be(originalBattery - consumptionAction)

  it should "act correctly" in:
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      consumptionAction,
      consumptionAction
    )
    var originalBattery = robot.batteryLevel
    robot.act()
    robot.position should be((0, 1))
    robot.batteryLevel should be(originalBattery - consumptionAction)

    robot.turn(Direction.East)
    originalBattery = robot.batteryLevel
    robot.act()
    robot.position should be((1, 1))
    robot.batteryLevel should be(originalBattery - consumptionAction)

    robot.turn(Direction.South)
    originalBattery = robot.batteryLevel
    robot.act()
    robot.position should be((1, 0))
    robot.batteryLevel should be(originalBattery - consumptionAction)

    robot.turn(Direction.West)
    originalBattery = robot.batteryLevel
    robot.act()
    robot.position should be((0, 0))
    robot.batteryLevel should be(originalBattery - consumptionAction)

  it should "recharge correctly" in:
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      consumptionAction,
      consumptionAction
    )
    var originalBattery = robot.batteryLevel
    // random actions to deplete battery
    robot.act()
    robot.turn(Direction.West)
    robot.act()
    robot.act()

    robot.batteryLevel should not be originalBattery
    originalBattery = robot.batteryLevel
    robot.recharge()
    robot.batteryLevel should not be originalBattery
    robot.batteryLevel should be(100)

  it should "stop when battery is 0" in:
    val massiveConsumptionActionTurn=25
    val massiveConsumptionActionAct=50
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      massiveConsumptionActionTurn,
      massiveConsumptionActionAct
    )

    robot.act()
    val firstPosition=robot.position
    robot.act()
    val lastPosition=robot.position
    lastPosition should not be firstPosition
    robot.act()
    robot.position should be(lastPosition)
    robot.batteryLevel should be (0)

    robot.recharge()

    robot.turn(Direction.South)
    robot.turn(Direction.West)
    robot.turn(Direction.East)
    robot.turn(Direction.North)
    robot.batteryLevel should be (0)
    robot.turn(Direction.South)
    robot.direction should be (Direction.North)

  it should "stop when action could cause battery to go below 0" in :
    val consumption = 99
    val robot = new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      consumption,
      consumption
    )

    robot.act()
    robot.batteryLevel should be (1)
    robot.act()
    robot.batteryLevel should be (1)

    robot.recharge()
    robot.turn(Direction.South)
    robot.batteryLevel should be (1)
    robot.turn(Direction.North)
    robot.batteryLevel should be (1)

  it should "require positive battery consumption in actions" in :
    a[IllegalArgumentException] should be thrownBy new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      -1,
      1
    )
    a[IllegalArgumentException] should be thrownBy new RobotWithBattery(
      SimpleRobot((0, 0), Direction.North),
      -1
    )
