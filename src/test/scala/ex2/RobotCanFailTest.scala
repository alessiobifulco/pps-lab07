package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotCanFailTest extends AnyFlatSpec with Matchers{

  "A Robot that Can Fail" should "not move if probability is lower than 50" in:

    var r = RobotCanFail(SimpleRobot((0, 0), Direction.North), 50)
    while (r.probability >= 50) {
      r = RobotCanFail(SimpleRobot((0, 0), Direction.North), 50)
    }

    val initialPos = r.position
    r.act()
    r.position shouldBe initialPos

  it should "move if probability is 50 or greater" in:
    var r = RobotCanFail(SimpleRobot((0, 0), Direction.North), 50)
    while (r.probability < 50) {
      r = RobotCanFail(SimpleRobot((0, 0), Direction.North), 50)
    }

    val initialPos = r.position
    r.act()

    r.position should not be initialPos
}
