package net.invalidkeyword.scaladiagrams

import scala.util.parsing.combinator._

object ScalaSourceParser extends RegexParsers with RunParser {

  override def skipWhitespace = true
  
  lazy val wordExp = regex("""\w+""".r) ^^ {case word => word}
  
  val wordClass = "class"
  val wordTrait = "trait"
  val wordWith = "with"
  val wordExtends = "extends"
  
  def classGroup = wordClass~wordExp~repsep(withGroup, " ") ^^ {case wordClass~name~withs => CLASS(name,withs)}
  def traitGroup = wordTrait~wordExp ^^ {case wordTrait~name => TRAIT(name)} 
  def withGroup = (wordExtends|wordWith)~wordExp ^^ {case pre~name => WITH(name)}
  
  def root = (classGroup|traitGroup)
  
  type RootType = EXP
}

trait RunParser {
  this: RegexParsers =>
    type RootType
  def root: Parser[RootType]
  def run(in: String): ParseResult[RootType] = parseAll(root, in) 
}

case class CLASS(name : String, withs : {}) extends EXP
case class TRAIT(name : String) extends EXP
case class WITH(name : String) extends EXP
abstract class EXP
