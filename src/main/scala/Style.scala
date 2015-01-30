package org.draegisoft.sonny

import Css.{Specificity, Stylesheet}
import Dom.Node

object Style {
  type PropertyMap = Map[String, Value]
  type StyledNode = NTree[(NodeType, PropertyMap)]
  type MatchedRule = (Selector, Rule)

  def styleTree(root: Node, sheet: Stylesheet): StyledNode = {
    def style (n: NodeType): (NodeType, PropertyMap) = n match {
      case Text(_) => (n, Map.empty)
      case Element(data) => (n, specifiedValues(data, sheet))
    }
     
    root map (style)
  }

  private def specifiedValues(data: ElementData, sheet: Stylesheet): PropertyMap = {
    val sortedRules = matchingRules(data, sheet).sortBy(_._1)(SelectorOrdering) 
    def expand(matchedRule: MatchedRule): List[(String, Value)] = matchedRule._2 match {
      case Rule(_, declarations) => declarations map { 
                                      case Declaration(name, value) => (name -> value) }}
    (sortedRules map (expand) flatten) toMap
  }

  private def matchingRules(data: ElementData, 
                            sheet: Stylesheet): List[MatchedRule] = 
    sheet.foldLeft (List.empty[MatchedRule]) { 
      (list: List[MatchedRule], rule: Rule) =>
        matchRule (data, rule) match {
          case Some(matchedRule) => matchedRule :: list
          case None => list
        }
    }

  private def matchRule(data: ElementData, rule: Rule): Option[MatchedRule] = {
    val matchingSelector = (rule selector) find (s => matches(data, s))
    matchingSelector map (s => (s, rule))
  } 

  private def matches(data: ElementData, selector: Selector): Boolean = 
    selector match {
      case Simple(_, _, _) => matchSimple(data, selector)
    }

  private def matchSimple(data: ElementData, simple: Selector): Boolean =
    simple match {
      case Simple (None, None, c) => matchClasses(data, c)
      case Simple (Some(n), None, c) => matchNames(data, n) && 
                                        matchClasses(data, c)
      case Simple (None, Some(i), c) => matchId(data, i) && 
                                        matchClasses(data, c)
      case Simple (Some(n), Some(i), c) => matchNames(data, n) &&
                                           matchId(data, i) &&
                                           matchClasses(data, c)
    }

  private def matchNames(data: ElementData, name: String): Boolean = 
    name == data.tag

  private def matchId(data: ElementData, id: String): Boolean = 
    id == data.findID

  private def matchClasses( data: ElementData
                          , classes: List[String]): Boolean = {
    val cls: Set[String] = data.classes
    classes match {
      case Nil => true
      case (c::cs) => (data.classes contains (c)) && matchClasses(data, cs)
    }
  }

  // added for Box implementation
  def value(styledNode: StyledNode, name: String): Option[Value] = styledNode match {
    case NTree((node, propertyMap), _) => propertyMap get(name)
    case _ => None
  }

  def display(styledNode: StyledNode): Display = value(styledNode, "display") match {
    case Some(Keyword("block")) => Block
    case Some(Keyword("none"))  => DisplayNone
    case _                      => Inline
  }
 
  def lookup(styledNode: StyledNode, keys: List[String], default: Value): Value = {
    keys.foldLeft (default) { case (d, k) => value(styledNode, k) match {
      case Some(v) => return v // we want the very first occurrence
      case _       => d
    }}
  }

}

// added for block implementation
sealed abstract class Display

case object Inline      extends Display
case object Block       extends Display
case object DisplayNone extends Display
