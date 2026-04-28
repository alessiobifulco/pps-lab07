package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedTest extends AnyFlatSpec with Matchers:

  "A RobotRepeated" should "move exactly n times when act() is called once" in:
    val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), 3)
    robot.act()
    robot.position shouldBe (0, 3)

  it should "not move at all if nReps is 0" in:
    val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), 0)
    robot.act()
    robot.position shouldBe (0, 0)