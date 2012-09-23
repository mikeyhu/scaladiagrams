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
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(RELATED("bill")))))
    }
        
    it("should parse a class with an extends") {
      val result = ScalaSourceParser.run("class bob extends bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(RELATED("bill")))))
    }
    
    it("should parse a class with an extends and some withs") {
      val result = ScalaSourceParser.run("class bob extends bill with peter with paul")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(RELATED("bill"),RELATED("peter"),RELATED("paul")))))
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
      ScalaSourceParser.filter(result.get) should be(List( CLASS("bob",List()),TRAIT("bill",List(RELATED("peter"))) ))
    }
    
    it("should parse a class with a self: ") {
      val result = ScalaSourceParser.run("class bob with peter { self: abc with xyz => ")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List( CLASS("bob",List(RELATED("peter",self=false),RELATED("abc",self=true),RELATED("xyz",self=true))) ))
    }
    
    it("should parse a case class with params") {
      val result = ScalaSourceParser.run("case class bob (abc : String) extends bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CASE("bob",List(RELATED("bill")))))
    }
    
    it("a class should have a color") {
      val cl = CLASS("abc",List())
      cl.color should be("darkorange")
    }
    
    it("should parse a source file") {
      val input = scala.io.Source.fromFile("src/main/scala/net/invalidkeyword/scaladiagrams/ScalaSourceParser.scala").mkString
      val result = ScalaSourceParser.run(input)
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(
          OBJECT("ScalaSourceParser",List(RELATED("RegexParsers"),RELATED("RunParser"))),
          TRAIT("RunParser",List()),
          CASE("CASE",List(RELATED("TYPE"))),
          CASE("OBJECT",List(RELATED("TYPE"))),
          CASE("CLASS",List(RELATED("TYPE"))),
          CASE("TRAIT",List(RELATED("TYPE"))),
          CASE("RELATED",List(RELATED("KEYWORD"))),
          OBJECT("IGNORED",List(RELATED("KEYWORD"))),
          CLASS("TYPE",List(RELATED("KEYWORD"))),
          CLASS("KEYWORD",List())
      ))
    }
    
  }
}