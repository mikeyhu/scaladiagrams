package net.invalidkeyword.scaladiagrams

import scala.util.parsing.combinator._

object ScalaSourceParser extends RegexParsers with RunParser {

  override def skipWhitespace = true
  
  val ignored = """\S""".r ^^ {case ig => IGNORED}
  
  val wordExp= """\w+""".r ^^ {case word => word}
  
  val packageExp = """[\w.]+""".r

  val bracketOpen = "("
  val brackets = """\([^\)]*\)""".r
  val bracketClose = ")"

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

  val beforeArgumentType = """[\w\s]+:\s*""".r
  val afterArgumentType = """\s*=.+?(?=[\),])""".r
  val typeName = """[\w\[\]]+""".r ^^ {case word => word}
  val argumentSeparator = ","

  def packageGroup = wordPackage~packageExp ^^ {case pre~name => name}
    
  def classGroup : Parser[KEYWORD] = wordClass~wordExp~related ^^ {case pre~name~related => CLASS(name,related)}
  def traitGroup : Parser[KEYWORD] = wordTrait~wordExp~related ^^ {case pre~name~related => TRAIT(name,related)} 
  def objectGroup : Parser[KEYWORD] = wordObject~wordExp~related ^^ {case pre~name~related => OBJECT(name,related)} 
  def caseGroup : Parser[KEYWORD] = wordCase~wordExp~opt(brackets)~related ^^ {case pre~name~brackets~related => CASE(name,related)}
  
  def withGroup = (wordExtends|wordWith)~wordExp ^^ {case pre~name => RELATED(name)}
  def selfGroup = (wordSelf|wordExtends|wordWith)~wordExp ^^ {case pre~name => RELATED(name,true)}
  def optionalSelf = selfStart~>rep(selfGroup)<~selfEnd
  def argumentTypeGroup = beforeArgumentType ~> typeName <~ opt(afterArgumentType) <~ opt(argumentSeparator) ^^ {case name => RELATED(name)}
  def argumentListTypes = bracketOpen ~> rep(argumentTypeGroup) <~ bracketClose

  def related = opt(argumentListTypes)~rep(withGroup)~opt(optionalSelf) ^^ {case types~withs~self => types.getOrElse(Nil) ++ withs ++ self.getOrElse(Nil)}
    
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
  
  def filter(matches : List[KEYWORD]) : List[TYPE] = matches.asInstanceOf[List[TYPE]]
  
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
  
  def node = quoted(name) + " [style=filled, fillcolor=" + color + "]"
  override def toString = node + withs.map(a=> quoted(name) + " -> " + quoted(a.name) + a.relationType).mkString("\n  ","\n  ","\n")
  val color = "white"
    
  override lazy val hasChildren = withs.nonEmpty
  def children = withs
  
  def isParentOf(name : String) = children.exists(a => a.name == name)
}

abstract class KEYWORD {
  lazy val hasChildren = false
  def name = ""

  def quoted(string : String) = "\"" + string + "\""
}
