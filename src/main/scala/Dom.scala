package org.draegisoft.sonny

object Dom {

  type AttributeMap = Map[String, String]
  type Node = NTree[NodeType]

  def elem (name: String, atts: AttributeMap): (List[Node] => Node) = 
    new NTree(Element(ElementData(name, atts)), _)

  def text(t: String): Node = new NTree(Text(t), Nil)
}

class NTree[+T](val a: T, val children: List[NTree[T]]) {
  def map[A] (f: T => A): NTree[A] = 
    new NTree(f(a), children map { t => t map (f) })

  override def equals (other: Any) = other match {
    case o: NTree[T] => a == o.a && children == o.children
    case _ => false
  }

  private def str(indent: Int): String = 
    " " * indent + a + '\n' + (children map (_.str (indent + 2)) mkString("\n")) 
  override def toString = str(0)
}

sealed class NodeType

case class Text(text: String) extends NodeType {
  override def toString = "Text: " + text
}

case class Element (data: ElementData) extends NodeType {
  override def toString = "Element: " + (data toString)
}

case class ElementData(tag: String, attributes: Dom.AttributeMap) {
  def findID = findAttr ("id")
  def findAttr (attr: String) = attributes get (attr)
  def classes = (findAttr ("class")) match {
    case None => Set.empty
    case Some (s) => s.split(' ').toSet
  }

  override def toString = 
    "%s [%s]".format (tag, attributes map { case (k, v) => k + '=' + v} mkString(", "))
}
