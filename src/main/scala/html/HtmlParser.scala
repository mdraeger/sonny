package org.draegisoft.sonny.html

import scala.util.parsing.combinator._

import org.draegisoft.sonny.{Dom, Element, Text}

class HtmlParser extends RegexParsers {
    import Dom.{AttributeMap, Node}

    override def skipWhitespace = false
    
    def parseHtml: Parser[Node] = ???

    def parseElement: Parser[Node] = openTag ~ content ~ endTag >> mkElement
    
    def parseText: Parser[Text] = charData ^^ Text 

    private def attributes: Parser[AttributeMap] = (space ~> attribute *)  ^^ (_.toMap)

    private def attribute: Parser[(String, String)] = 
        (name <~ equals) ~ string ^^ {case (k ~ v) => (k -> v)}

    private def content: Parser[List[Node]] = 
        (parseText?) ~ (parseElement ~(parseText?) *) ^^ {
          case (Some(t1: Text), List(_)) => List(t1)
        }

    private def openTag = "<" ~> name ~ attributes <~ (space?) <~ ">"

    private def endTag = "</" ~> name <~ (space?) <~ ">"

    private def equals       = (space?) ~ "=" ~ (space?)
    private def string       = doubleString | singleString
    private def charData     = "[^<]+".r
    private def space        = """\s+""".r
    private def name         = """(:|\w)((\-|\.|\d|:\w))*""".r
    private def doubleString = "\"" ~> "[^\"]*".r <~ "\""
    private def singleString = "'" ~> "[^']*".r <~ "'"

    private def mkElement: (String~AttributeMap~List[Node]~String => Parser[Node]) = ???
    /*{
      case startName ~ atts ~ children ~ endName =>
        if (startName == endName)
          success (Dom.elem (startName, atts)(children))
        else 
          err("tag mismatch")
      } */
}
