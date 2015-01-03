package org.draegisoft.sonny.test

import org.draegisoft.sonny.{Dom, Element, Text}
import org.draegisoft.sonny.html.HtmlParser

import org.scalatest._

import java.io._

import scala.util.parsing.input.CharSequenceReader

import TestValues._

class HtmlParserSpec extends HtmlParser with FlatSpecLike with Matchers{

  "The parser" should "parse the text \"candygram\" correctly" in {
    implicit val parserToTest = parseText
    parsing(testText) should equal (Dom.text(testText))
  }

  "The parser" should "parse the element <p ham=\"doctor\">sup</p> correctly" in {
    implicit val parserToTest = parseElement
    parsing(testP) should equal (testElem)
  }

  "The parser" should """parse the html document 
    <html>
       <head>
          <title>Test</title>
       </head>
       <p class=\"inner\">Hello, <span id="name">world!</span></p>
       <p class=\"inner\">Goodbye!</p>
    </html> correctly""" in {
    implicit val parserToTest = parseHtml
    parsing(testHtml) should equal (dom)
  }


  private def assertFail[T](input: String)(implicit p: Parser[T]) = {
    an [IllegalArgumentException] should be thrownBy { parsing(input)(p)} 
  }

  private def parsing[T](s: String)(implicit p: Parser[T]): T = {
    val phraseParser = phrase(p)
    val input = new CharSequenceReader(s)

    phraseParser(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => 
        throw new IllegalArgumentException(
          "Could not parse '%s': %s".format(s,msg)
        )
    }
  }
}
