package net.invalidkeyword.scaladiagrams

import scala.util.parsing.combinator._

object ScalaSourceParser extends RegexParsers with RunParser {

  override def skipWhitespace = true
  
  val ignored = regex("""\S""".r) ^^ {case ig => IGNORED}
  
  val wordExp= regex("""\w+""".r) ^^ {case word => word}
  
  val packageExp = regex("""[\w.]+""".r)
  
  val brackets = regex("""\([^\)]*\)""".r)
  
  val wordPackage = "package "
  val wordClass = "class "
  val wordCase = "case class "
  val wordTrait = "trait "
  val wordObject = "object "
  val wordWith = "with "
  val wordExtends = "extends "
  
  val selfStart = """\{\s*""".r
  val selfEnd = "=>"
  val wordSelf = "self:"
  
  def packageGroup = wordPackage~packageExp ^^ {case pre~name => name}
    
  def classGroup : Parser[KEYWORD] = wordClass~wordExp~opt(brackets)~related ^^ {case pre~name~brackets~related => CLASS(name,related)}
  def traitGroup : Parser[KEYWORD] = wordTrait~wordExp~related ^^ {case pre~name~related => TRAIT(name,related)} 
  def objectGroup : Parser[KEYWORD] = wordObject~wordExp~related ^^ {case pre~name~related => OBJECT(name,related)} 
  def caseGroup : Parser[KEYWORD] = wordCase~wordExp~opt(brackets)~related ^^ {case pre~name~brackets~related => CASE(name,related)} 
  
  def withGroup = (wordExtends|wordWith)~wordExp ^^ {case pre~name => RELATED(name)}
  def selfGroup = (wordSelf|wordExtends|wordWith)~wordExp ^^ {case pre~name => RELATED(name,true)}
  def optionalSelf = selfStart~>rep(selfGroup)<~selfEnd
  
  def related = rep(withGroup)~opt(optionalSelf) ^^ {case withs~self => withs ++ self.getOrElse((List()))}
    
  def parsable : Parser[List[KEYWORD]] = opt(packageGroup)~rep(caseGroup|classGroup|traitGroup|objectGroup|ignored) ^^ {
    case pack~groups => 
      groups.filter(i=>i != IGNORED).map{
        case CLASS(n,r,_) => CLASS(n,r,pack.getOrElse(""))
        case TRAIT(n,r,_) => TRAIT(n,r,pack.getOrElse(""))
        case OBJECT(n,r,_) => OBJECT(n,r,pack.getOrElse(""))
        case CASE(n,r,_) => CASE(n,r,pack.getOrElse(""))
      }
  }
 
  def root = parsable 
  
  type RootType = List[KEYWORD]
  
  def filter(matches : List[KEYWORD]) : List[TYPE] = matches.filter(r => r!=IGNORED).asInstanceOf[List[TYPE]]
  
}

trait RunParser {
  this: RegexParsers =>
    type RootType
  def root: Parser[RootType]
  def run(in: String): ParseResult[RootType] = parseAll(root, in)
}

case class CASE(override val name : String, withs : List[RELATED], pack : String="") extends TYPE(name,withs,pack) {
  override val color = "burlywood"
}
case class OBJECT(override val name : String, withs : List[RELATED], pack : String="") extends TYPE(name,withs,pack) {
  override val color = "gold"
}
case class CLASS(override val name : String, withs : List[RELATED], pack : String="") extends TYPE(name,withs,pack) {
  override val color = "darkorange"
}

case class TRAIT(override val name : String, withs : List[RELATED], pack : String="") extends TYPE(name,withs,pack) {
  override val color = "cadetblue"
}

case class RELATED(override val name : String, self : Boolean = false) extends KEYWORD {
  def relationType = if(self) " [style=dashed];" else ";"
}

object IGNORED extends KEYWORD

abstract class TYPE(override val name : String, withs : List[RELATED], pack : String ="") extends KEYWORD {
  
  def node = name + " [style=filled, fillcolor=" + color + "]"
  override def toString = node + withs.map(a=> name + " -> " + a.name + a.relationType).mkString("\n  ","\n  ","\n")
  val color = "white"
    
  override lazy val hasChildren = withs.size > 0
  def children = withs
  
  def isParentOf(name : String) = children.find(a => a.name==name).isDefined
}

abstract class KEYWORD {
  lazy val hasChildren = false
  def name = ""
}
