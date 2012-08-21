package net.invalidkeyword.scaladiagrams

import scala.util.parsing.combinator._

object ScalaSourceParser extends RegexParsers with RunParser {

  override def skipWhitespace = true
  
  lazy val wordExp = regex("""\w+""".r) ^^ {case word => word}
  
  lazy val wordClass = regex("class".r)
  lazy val wordTrait = regex("trait".r)
  lazy val wordWith = regex("""[w][i][t][h]""".r)
  lazy val wordExtends = regex("""extends""".r)
  
 
  
  //def expAnd : Parser[EXP]= (bracketedGroup|wordExp)~opt(boolAnd)~exp ^^ { case pre~boolAnd~post => AND(pre,post) }
  //def expOr : Parser[EXP]= (bracketedGroup|wordExp)~boolOr~exp ^^ { case pre~boolOr~post => OR(pre,post) }
  	
  def classGroup = wordClass~wordExp ^^ {case wordClass~name => CLASS(name)}
  def traitGroup = wordTrait~wordExp ^^ {case wordTrait~name => TRAIT(name)} 
  
  def root = (classGroup|traitGroup)
  
  type RootType = EXP
}

trait RunParser {
  this: RegexParsers =>
    type RootType
  def root: Parser[RootType]
  def run(in: String): ParseResult[RootType] = parseAll(root, in) 
}

case class CLASS(name : String) extends EXP
case class TRAIT(name : String) extends EXP
abstract class EXP
