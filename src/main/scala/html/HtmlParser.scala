package org.draegisoft.sonny.html

import scala.util.parsing.combinator._

import org.draegisoft.sonny.{Dom, Element, SonnyParser, Text}

class HtmlParser extends SonnyParser {
    import Dom.{AttributeMap, Node}

    def parseHtml: Parser[Node] = (eol *) ~> (space?) ~> parseElement

    def parseElement: Parser[Node] = (space?) ~>
                                     openTag ~ 
                                     (parseNode *) ~ 
                                     endTag <~
                                     (space?) <~
                                     (eol *) >> mkElement
    
    def parseText: Parser[Node] = charData ^^ Dom.text 

    private def parseNode: Parser[Node] = parseElement | parseText

    private def attributes: Parser[AttributeMap] = 
      (space ~> attribute *)  ^^ (_.toMap)

    private def attribute: Parser[(String, String)] = 
        (name <~ equals) ~ string ^^ {case (k ~ v) => (k -> v)}

    private def openTag = (eol *) ~> "<" ~> name ~ attributes <~ (space?) <~ ">" <~ (eol *)

    private def endTag = (eol *) ~> "</" ~> name <~ (space?) <~ ">" <~ (eol *)

    private def mkElement: (String~AttributeMap~List[Node]~String => Parser[Node]) = {
      case startName ~ atts ~ children ~ endName =>
        if (startName == endName)
          success (Dom.elem (startName, atts)(children))
        else 
          err("tag mismatch")
      }
}
