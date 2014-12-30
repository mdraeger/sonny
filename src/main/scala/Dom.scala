package org.draegisoft.sonny

object Dom {

  type AttributeMap = Map[String, String]
  type Node = NTree[NodeType]

  def elem (name: String, atts: AttributeMap): (List[Node] => Node) = 
    new NTree(Element(ElementData(name, atts)), _)

  def text(t: String): Node = new NTree(Text(t), Nil)
}

class NTree[T](val a: T, val children: List[NTree[T]]) 

sealed class NodeType

case class Text(text: String) extends NodeType
case class Element (data: ElementData) extends NodeType

case class ElementData(tag: String, attributes: Dom.AttributeMap) {
  def findID = findAttr ("id")
  def findAttr (attr: String) = attributes get (attr)
  def classes = (findAttr ("class")) match {
    case None => Set.empty
    case Some (s) => s.split(' ').toSet
  }
}
