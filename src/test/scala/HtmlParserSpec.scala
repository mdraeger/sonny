package org.draegisoft.sonny.test

import org.draegisoft.sonny.html.HtmlParser

import java.io._

import scala.util.parsing.input.CharSequenceReader

import org.scalatest._

class HtmlParserSpec extends HtmlParser with FlatSpecLike with Matchers{
  
  "The parser" should "parse the text \"candygram\" correctly" in {
    implicit val parserToTest = parseText
    val testText = "candygram"
    parsing(testText) should equal (testText)
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
