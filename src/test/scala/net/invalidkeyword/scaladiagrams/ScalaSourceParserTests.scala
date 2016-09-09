package net.invalidkeyword.scaladiagrams

import org.scalatest.{FlatSpec, Matchers}

class ScalaSourceParserTests extends FlatSpec with Matchers {

  "The ScalaSourceParser" should " parse a class" in {
      val result = ScalaSourceParser.run("class bob")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List())))
    }
    
    it should " parse a trait" in {
      val result = ScalaSourceParser.run("trait Cat")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(TRAIT("Cat",List())))
    }
    
    it should " fail to parse some other text" in {
      val result = ScalaSourceParser.run("bob is a cat")
      ScalaSourceParser.filter(result.get) should be(List())
    }

    it should " parse a class arguments with default values" in {
      val result = ScalaSourceParser.run("class Bob (a : String = \"string\")")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("Bob",List(RELATED("String")))))
    }

    it should " parse a class with type parameter" in {
      val result = ScalaSourceParser.run("class Bob(val bill: Option[Bill] = None, will: Will)")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("Bob",List(RELATED("Option[Bill]"),RELATED("Will")))))
    }

    it should " parse a class with newline in argument list" in {
      val result = ScalaSourceParser.run(
        """class Bob(a: Bill,
          |val b: Will)""".stripMargin)
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("Bob",List(RELATED("Bill"),RELATED("Will")))))
    }

    it should " parse a class with arguments and extends" in {
      val result = ScalaSourceParser.run("class Bob(a: Bill, b: Will) extends DuckBill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("Bob",List(RELATED("Bill"),RELATED("Will"),RELATED("DuckBill")))))
    }
    
    it should " parse a class with an with" in {
      val result = ScalaSourceParser.run("class bob with bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(RELATED("bill")))))
    }
        
    it should " parse a class with an extends" in {
      val result = ScalaSourceParser.run("class bob extends bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(RELATED("bill")))))
    }
    
    it should " parse a class with an extends and some withs" in {
      val result = ScalaSourceParser.run("class bob extends bill with peter with paul")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(RELATED("bill"),RELATED("peter"),RELATED("paul")))))
    }
    
    it should " parse a class after some other text" in {
      val result = ScalaSourceParser.run("this is a test class bob")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List())))
    }
    
    it should " parse a class before some other text" in {
      val result = ScalaSourceParser.run("class bob and some more stuff")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List())))
    }
    
    it should " parse a class with some other text" in {
      val result = ScalaSourceParser.run("abc some class bob and some trait bill with peter more stuff")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List( CLASS("bob",List()),TRAIT("bill",List(RELATED("peter"))) ))
    }
    
    it should " parse a class with a self: " in {
      val result = ScalaSourceParser.run("class bob with peter { self: abc with xyz => ")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List( CLASS("bob",List(RELATED("peter",self=false),RELATED("abc",self=true),RELATED("xyz",self=true))) ))
    }
    
    it should " parse a trait with a self: " in {
      val result = ScalaSourceParser.run("trait bob with peter { self: abc with xyz => ")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List( TRAIT("bob",List(RELATED("peter",self=false),RELATED("abc",self=true),RELATED("xyz",self=true))) ))
    }
    
    it should " parse a case class with params" in {
      val result = ScalaSourceParser.run("case class bob (abc : String) extends bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CASE("bob",List(RELATED("bill")))))
    }
    
    it should " parse a package with a class" in {
      val result = ScalaSourceParser.run("package bill.peter\n class bob (abc: String) extends bill")
      result.successful should be(true)
      ScalaSourceParser.filter(result.get) should be(List(CLASS("bob",List(RELATED("String"),RELATED("bill")),"bill.peter")))
    }
    
    it should " a class should have a color" in {
      val cl = CLASS("abc",List())
      cl.color should be("darkorange")
    }
    
    //This test is based on the source code of ScalaSourceParser.scala so is likely to break when that file is changed...
    it should " parse a source file" in {
      val input = scala.io.Source.fromFile("src/main/scala/net/invalidkeyword/scaladiagrams/ScalaSourceParser.scala").mkString
      val result = ScalaSourceParser.run(input)
      result.successful should be(true)
      ScalaSourceParser.filter(result.get).head should be(
          OBJECT("ScalaSourceParser",List(RELATED("RegexParsers"),RELATED("RunParser")),"net.invalidkeyword.scaladiagrams")
      )
    }
    
    it should " output an type in DOT format" in {
      val cl = CLASS("abc",List(RELATED("def")))
      cl.toString() should be("\"abc\" [style=filled, fillcolor=darkorange]\n  \"abc\" -> \"def\";\n")
    }
    
    it should " output a type in DOT format with a dashed line for self-types" in {
      val cl = CLASS("abc",List(RELATED("def",true)))
      cl.toString() should be("\"abc\" [style=filled, fillcolor=darkorange]\n  \"abc\" -> \"def\" [style=dashed];\n")
    }
    
}