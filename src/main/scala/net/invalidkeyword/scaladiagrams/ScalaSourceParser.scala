package net.invalidkeyword.scaladiagrams

import scala.util.parsing.combinator._

object ScalaSourceParser extends RegexParsers with RunParser {

  override def skipWhitespace = true
  
  

  val ignored = regex("""\S""".r) ^^ {case ig => IGNORED()}
  
  val wordExp= regex("""\w+""".r) ^^ {case word => word}
  
  val brackets = regex("""\([^\)]*\)""".r)
  
  val wordClass = "class "
  val wordCase = "case class "
  val wordTrait = "trait "
  val wordObject = "object "
  val wordWith = "with "
  val wordExtends = "extends "
  
  def classGroup : Parser[EXP] = wordClass~wordExp~opt(brackets)~rep(withGroup) ^^ {case pre~name~brackets~withs => CLASS(name,withs)}
  def traitGroup : Parser[EXP]= wordTrait~wordExp~rep(withGroup) ^^ {case pre~name~withs => TRAIT(name,withs)} 
  def objectGroup : Parser[EXP]= wordObject~wordExp~rep(withGroup) ^^ {case pre~name~withs => OBJECT(name,withs)} 
  def caseGroup : Parser[EXP]= wordCase~wordExp~opt(brackets)~rep(withGroup) ^^ {case pre~name~brackets~withs => CASE(name,withs)} 
  
  def withGroup = (wordExtends|wordWith)~wordExp ^^ {case pre~name => WITH(name)}
  
  def parsable : Parser[List[EXP]] = rep(caseGroup|classGroup|traitGroup|objectGroup|ignored)
  
  def root = parsable 
  
  type RootType = List[EXP]
  
  def filter(matches : List[EXP]) = matches.filter(r => r!=IGNORED())
  
}

trait RunParser {
  this: RegexParsers =>
    type RootType
  def root: Parser[RootType]
  def run(in: String): ParseResult[RootType] = parseAll(root, in)
}

case class CASE(name : String, withs : {}) extends EXP
case class OBJECT(name : String, withs : {}) extends EXP
case class CLASS(name : String, withs : {}) extends EXP
case class TRAIT(name : String, withs : {}) extends EXP
case class WITH(name : String) extends EXP
case class IGNORED() extends EXP
abstract class EXP
