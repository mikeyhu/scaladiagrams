package net.invalidkeyword.scaladiagrams

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class ScalaSourceParserTests extends Spec with ShouldMatchers  {

  describe("The ScalaSourceParser") {
    it("should parse a class") {
      val result = ScalaSourceParser.run("class bob")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List())))
    }
    
    it("should parse a trait") {
      val result = ScalaSourceParser.run("trait Cat")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(TRAIT("Cat",List())))
    }
    
    it("should fail to parse some other text") {
      val result = ScalaSourceParser.run("bob is a cat")
      ScalaSourceParser.filter(result.get) should be(List())
    }   
    
    it("should parse a class with an with") {
      val result = ScalaSourceParser.run("class bob with bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(WITH("bill")))))
    }
        
    it("should parse a class with an extends") {
      val result = ScalaSourceParser.run("class bob extends bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(WITH("bill")))))
    }
    
    it("should parse a class with an extends and some withs") {
      val result = ScalaSourceParser.run("class bob extends bill with peter with paul")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(WITH("bill"),WITH("peter"),WITH("paul")))))
    }
    
    it("should parse a class after some other text") {
      val result = ScalaSourceParser.run("this is a test class bob")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List())))
    }
    
    it("should parse a class before some other text") {
      val result = ScalaSourceParser.run("class bob and some more stuff")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List())))
    }
    
    it("should parse a class with some other text") {
      val result = ScalaSourceParser.run("abc some class bob and some trait bill with peter more stuff")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List( CLASS("bob",List()),TRAIT("bill",List(WITH("peter"))) ))
    }
    
    it("should parse a case class with params") {
      val result = ScalaSourceParser.run("case class bob (abc : String) extends bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CASE("bob",List(WITH("bill")))))
    }
    
    it("should parse a source file") {
      val input = scala.io.Source.fromFile("/home/casper/scaladiagrams/src/main/scala/net/invalidkeyword/scaladiagrams/ScalaSourceParser.scala").mkString
      val result = ScalaSourceParser.run(input)
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(
          OBJECT("ScalaSourceParser",List(WITH("RegexParsers"),WITH("RunParser"))),
          TRAIT("RunParser",List()),
          CASE("CASE",List(WITH("EXP"))),
          CASE("OBJECT",List(WITH("EXP"))),
          CASE("CLASS",List(WITH("EXP"))),
          CASE("TRAIT",List(WITH("EXP"))),
          CASE("WITH",List(WITH("EXP"))),
          CASE("IGNORED",List(WITH("EXP"))),
          CLASS("EXP",List())
      ))
    }
    
  }
}