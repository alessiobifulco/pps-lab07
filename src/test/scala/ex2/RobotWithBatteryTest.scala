package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatteryTest extends AnyFlatSpec with Matchers:

  "A Robot with Battery" should "act have 0 batttry after 4 action" in:
    val robot = RobotWithBattery(SimpleRobot((0, 0), Direction.North))
    robot.act()
    robot.act()
    robot.act()
    robot.act()
    robot.batteryLevel shouldBe 0