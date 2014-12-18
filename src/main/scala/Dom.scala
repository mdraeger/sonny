package org.draegisoft.sonny

import java.nio.charset.StandardCharsets

case class Dom(val node: NTree[NodeType])

class NTree[T](a: T, children: List[NTree[T]]) 

sealed class NodeType

case class Text(text: String) extends NodeType
case class Element (data: ElementData) extends NodeType

case class ElementData(tag: String, attributes: Map[String, String]) {
  def findID = findAttr ("id")
  def findAttr (attr: String) = attributes get (attr)
  def classes = (findAttr ("class")) match {
    case None => Set.empty
    case Some (s) => s.split(' ').toSet
  }
}
