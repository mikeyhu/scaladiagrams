package net.invalidkeyword.scaladiagrams

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class ScalaSourceParserTests extends Spec with ShouldMatchers  {

  describe("The ScalaSourceParser") {
    it("should parse a class") {
      val result = ScalaSourceParser.run("class bob")
      result.successful should be(true)
      result.get should be(CLASS("bob"))
    }
    
    it("should parse a trait") {
      val result = ScalaSourceParser.run("trait Cat")
      result.successful should be(true)
      result.get should be(TRAIT("Cat"))
    }
    
    it("should fail to parse some other text") {
      val result = ScalaSourceParser.run("bob is a cat")
      result.successful should be(false)
    }    
  }
}