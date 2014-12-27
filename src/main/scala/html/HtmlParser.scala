package org.draegisoft.sonny.html

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import org.draegisoft.sonny.{Dom, Element, Text}

class HtmlParser extends JavaTokenParsers {
    
    def parseHtml: Parser[Dom] = ???

    def parseElement: Parser[Element] = ???
    
    def parseText: Parser[Text] = ???
}
