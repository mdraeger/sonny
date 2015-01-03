package org.draegisoft.sonny

import Css.Stylesheet
import Dom.Node

object Style {
   type PropertyMap = Map[String, Value]
   type StyledNode = NTree[(NodeType, PropertyMap)]

   def styleTree(node: Node, style: Stylesheet): StyledNode = ???

   def specifiedValues(data: ElementData, style: Stylesheet): PropertyMap = ???
}
