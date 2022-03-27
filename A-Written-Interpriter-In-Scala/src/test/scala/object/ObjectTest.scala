package `object`

import org.scalatest.FunSpec
import org.scalatest.Matchers.not
import org.scalatest.MustMatchers.convertToAnyMustWrapper

class ObjectTest extends FunSpec {
  describe("ObjectTest") {
    it("test string hash key") {
      val hello1 = HashKey("Hello World")
      val hello2 = HashKey("Hello World")
      val diff1 = HashKey("My name is Johnny")
      val diff2 = HashKey("My name is Johnny")
      hello1.value mustEqual hello2.value
      diff1.value mustEqual diff2.value
      hello1.value must not equal diff1.value
      hello1.objectType mustEqual STRING_OBJ
    }
    it("test boolean hash key") {
      val true1 = HashKey(true)
      val true2 = HashKey(true)
      val false1 = HashKey(false)
      true1.value mustEqual true2.value
      true1.value must not equal false1.value
      true1.objectType mustEqual BOOLEAN_OBJ
    }

    it("test integer hash key") {
      val one1 = HashKey(1)
      val one2 = HashKey(1)
      val two1 = HashKey(2)
      one1.value mustEqual one2.value
      one1.value must not equal two1.value
    }
  }
}
