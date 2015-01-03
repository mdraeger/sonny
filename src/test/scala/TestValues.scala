package org.draegisoft.sonny.test

import org.draegisoft.sonny._
import Style._

object TestValues {
  def text = Dom.text(_)
  
  // simple text for parsing
  val testText = "candygram"
  // simple element for parsing 
  val testP    = "<p ham=\"doctor\">sup</p>"
 
  // test document to be parsed into dom
  val testHtml = """
   <html>
      <head>
         <title>Test</title>
      </head>
      <p class="inner">Hello, <span id="name">world!</span></p>
      <p class="inner">Goodbye!</p>
   </html>"""

  // Parsing result for testP
  val testElem = Dom.elem("p", Map(("ham" -> "doctor")))(List(text("sup")))

  // DOM for testing
  val dom   = {
    val span  = Dom.elem("span",  Map(("id" -> "name")))    (List(text("world!")))
    val hello = Dom.text("Hello, ")
    val p1    = Dom.elem("p",     Map(("class" -> "inner")))(List(hello, span))
    val p2    = Dom.elem("p",     Map(("class" -> "inner")))(List(text("Goodbye!")))
    val title = Dom.elem("title", Map.empty)                (List(text("Test")))
    val head  = Dom.elem("head",  Map.empty)                (List(title))
    Dom.elem("html",  Map.empty) (List(head, p1, p2))
  }

  // Css to parse
  val testCss = """
h1, h2, h3 {margin: auto; color: #cc0000; }
div.note { margin-bottom: 20px; padding: 10px; }
#answer { display: none; }
"""

  // expected result for parsing testCss
  val testSheet = List(Rule( List( Simple (Some("h1"), None, Nil)
                                 , Simple (Some("h2"), None, Nil)
                                 , Simple (Some("h3"), None, Nil))
                           , List( Declaration("margin", Keyword("auto"))
                                 , Declaration("color", ColorValue(
                                               Color(204,0,0,255)))))
                      ,Rule( List( Simple (Some("div"), None, "note"::Nil))
                           , List( Declaration("margin-bottom", Length(20, Px()))
                                 , Declaration("padding", Length(10, Px()))))
                      ,Rule( List( Simple (None, Some("answer"), Nil))
                           , List( Declaration("display", Keyword("none")))))

  // css declarations for styled dom
  val css2 = List( Rule ( List ( Simple ( Some("head"), None, Nil))
                        , List ( Declaration ("margin", Keyword("auto"))
                               , Declaration ("color", (ColorValue(
                                                          Color(0,0,0,255))))))
                 , Rule ( List ( Simple ( Some("p"), None, List("inner")))
                        , List ( Declaration ("padding", Length(20, Px())))))

  // Styled dom 
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
}
