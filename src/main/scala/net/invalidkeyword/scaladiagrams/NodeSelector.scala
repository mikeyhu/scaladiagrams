package net.invalidkeyword.scaladiagrams

class NodeSelector(nodes : List[EXP]) {

  def findNode(name : String) = nodes.find(_.name == name).asInstanceOf[Option[WITHABLE]]
  def findNode(w : WITH) = nodes.find(_.name == w.name).asInstanceOf[Option[WITHABLE]]
 
  def selectNodes (parent : WITHABLE) : Set[WITHABLE]= {
    val childset = parent.children.map(a => findNode(a.name)).flatten.toSet
    childset.map(selectNodes(_)).flatten + parent
  }
  
  def nodesWithParentsOrChildren = {
    nodes.filter(n => n.hasChildren || nodes.filter(b => b.asInstanceOf[WITHABLE].isParentOf(n.name)).size > 0).toSet
  }
}