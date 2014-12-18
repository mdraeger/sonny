package org.draegisoft.sonny.html

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import org.draegisoft.sonny._

object HtmlParser extends Parsers {
    
    def parseHtml: Parser[Dom] = ???

    def parseElement: Parser[Element] = ???
    
    def parseText: Parser[Text] = ???
}
