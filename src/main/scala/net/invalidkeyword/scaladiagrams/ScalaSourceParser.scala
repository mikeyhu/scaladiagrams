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
  
  val selfStart = """\{\s*""".r
  val selfEnd = "=>"
  val wordSelf = "self:"
  
  def classGroup : Parser[EXP] = wordClass~wordExp~opt(brackets)~rep(withGroup)~opt(optionalSelf) ^^ {case pre~name~brackets~withs~self => CLASS(name,withs ++ self.getOrElse((List())))}
  def traitGroup : Parser[EXP] = wordTrait~wordExp~rep(withGroup) ^^ {case pre~name~withs => TRAIT(name,withs)} 
  def objectGroup : Parser[EXP] = wordObject~wordExp~rep(withGroup) ^^ {case pre~name~withs => OBJECT(name,withs)} 
  def caseGroup : Parser[EXP] = wordCase~wordExp~opt(brackets)~rep(withGroup) ^^ {case pre~name~brackets~withs => CASE(name,withs)} 
  
  def withGroup = (wordExtends|wordWith)~wordExp ^^ {case pre~name => WITH(name)}
  def selfGroup = (wordSelf|wordExtends|wordWith)~wordExp ^^ {case pre~name => WITH(name,true)}
  
  def optionalSelf = selfStart~>rep(selfGroup)<~selfEnd 
  
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

case class CASE(override val name : String, withs : List[WITH]) extends WITHABLE(name,withs) {
  override val color = "burlywood"
}
case class OBJECT(override val name : String, withs : List[WITH]) extends WITHABLE(name,withs) {
  override val color = "gold"
}
case class CLASS(override val name : String, withs : List[WITH]) extends WITHABLE(name,withs) {
  override val color = "darkorange"
}

case class TRAIT(override val name : String, withs : List[WITH]) extends WITHABLE(name,withs) {
  override val color = "cadetblue"
}

case class WITH(override val name : String, self : Boolean = false) extends EXP

case class IGNORED() extends EXP

abstract class WITHABLE(override val name : String, withs : List[WITH]) extends EXP {
  
  def node = name + " [style=filled, fillcolor=" + color + "]"
  override def toString = node + "\n" + withs.map(a=> name + " -> " + a.name).mkString(";\n")
  val color = "white"
    
  override lazy val hasChildren = withs.size > 0
  def children = withs
  
  def isParentOf(name : String) = children.find(a => a.name==name).isDefined
}

abstract class EXP {
  lazy val hasChildren = false
  def name = ""
}
