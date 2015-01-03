package org.draegisoft.sonny.test

import org.draegisoft.sonny._
import Dom._
import Style._

import org.scalatest._

import TestValues._

class StyleSpec extends FlatSpec with Matchers{

  val css2 = List( Rule ( List ( Simple ( Some("head"), None, Nil))
                        , List ( Declaration ("margin", Keyword("auto"))
                               , Declaration ("color", (ColorValue(
                                                          Color(0,0,0,255))))))
                 , Rule ( List ( Simple ( Some("p"), None, List("inner")))
                        , List ( Declaration ("padding", Length(20, Px())))))

  val styletree: StyledNode = {
    val rule1 = Map(("margin", Keyword("auto"))
                   , ("color", ColorValue(Color(0,0,0,255))))
    val rule2 = Map(("padding", Length(17, Px())))
    val propEmpty = Map.empty[String, Value]
    val attrEmpty = Map.empty[String, String]
    val test_ = new NTree((Text("Test"), propEmpty), Nil)
    val title = new NTree((Element(ElementData("title", attrEmpty)), propEmpty), List(test_))
    val head = new NTree((Element(ElementData("head", attrEmpty)), rule1), List(title))
    val hello = new NTree((Text("Hello, "), propEmpty), Nil)
    val world = new NTree((Text("world!"), propEmpty), Nil)
    val span = new NTree((Element(ElementData("span", Map(("id", "name")))), propEmpty), List(world))
    val goodbye = new NTree((Text("Goodbye!"), propEmpty), Nil)
    val p1 = new NTree((Element(ElementData("p", Map(("class", "inner")))), rule2), List(hello, span))
    val p2 = new NTree((Element(ElementData("p", Map(("class", "inner")))), rule2), List(goodbye))
    new NTree((Element(ElementData("html", attrEmpty)), propEmpty), List(head, p1, p2))
  }
 
  "StyleSpec" should "do something useful" in {
     styletree equals (styleTree(dom, css2))
  }
}
