package org.draegisoft.sonny.test

import org.draegisoft.sonny.{Dom, Element, Text}
import org.draegisoft.sonny.html.HtmlParser

import java.io._

import scala.util.parsing.input.CharSequenceReader

import org.scalatest._

class HtmlParserSpec extends HtmlParser with FlatSpecLike with Matchers{

  def text = Dom.text (_)
  val testText = "candygram"
  val testP    = "<p ham=\"doctor\">sup</p>"
  val testHtml = """
   <html>
      <head>
         <title>Test</title>
      </head>
      <p class="inner">Hello, <span id="name">world!</span></p>
      <p class="inner">Goodbye!</p>
   </html>"""
  
  "The parser" should "parse the text \"candygram\" correctly" in {
    implicit val parserToTest = parseText
    parsing(testText) should equal (text(testText))
  }

  "The parser" should "parse the element <p ham=\"doctor\">sup</p> correctly" in {
    implicit val parserToTest = parseElement
    val testElem = Dom.elem("p", Map(("ham" -> "doctor")))(List(text("sup")))
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

    val dom   = {
      val span  = Dom.elem("span",  Map(("id" -> "name")))    (List(text("world!")))
      val hello = Dom.text("Hello, ")
      val p1    = Dom.elem("p",     Map(("class" -> "inner")))(List(hello, span))
      val p2    = Dom.elem("p",     Map(("class" -> "inner")))(List(text("Goodbye!")))
      val title = Dom.elem("title", Map.empty)                (List(text("Test")))
      val head  = Dom.elem("head",  Map.empty)                (List(title))
    Dom.elem("html",  Map.empty) (List(head, p1, p2))
    }

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
