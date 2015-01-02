package org.draegisoft.sonny.test

import org.draegisoft.sonny._

import java.io._

import scala.util.parsing.input.CharSequenceReader

import org.scalatest._

class CssParserSpec extends CssParser with FlatSpecLike with Matchers{

  val testCss = """
h1, h2, h3 {margin: auto; color: #cc0000; }
div.note { margin-bottom: 20px; padding: 10px; }
#answer { display: none; }
"""

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
  
  "The parser" should "parse valid css correctly" in {
    implicit val parserToTest = parseCss
    parsing(testCss) should equal (testSheet)
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
          "Could not parse '%s'%n%s".format(s,msg)
        )
    }
  }
}
