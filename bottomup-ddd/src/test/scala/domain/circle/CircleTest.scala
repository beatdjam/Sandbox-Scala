package domain.circle

import domain.user.UserId
import org.scalatest.wordspec.AnyWordSpec

class CircleTest extends AnyWordSpec {
  "CircleTest" when {
    "#join" must {
      "上限を超えてない場合" should {
        "新しいメンバーを追加したオブジェクトが返却される" in {
          val circle = Circle(
            name = CircleName("Test"),
            owner = UserId("Test"),
            members = Seq(UserId("Test2"), UserId("Test3"))
          )
          val copied = circle.join(UserId("Test4"))
          assert(copied.members.contains(UserId("Test4")))
          assert(copied.members.size == 3)
        }
      }
    }
  }
}
