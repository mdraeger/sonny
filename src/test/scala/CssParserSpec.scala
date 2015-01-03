package org.draegisoft.sonny.test

import org.draegisoft.sonny._

import java.io._

import org.scalatest._

import scala.util.parsing.input.CharSequenceReader

import TestValues._

class CssParserSpec extends CssParser with FlatSpecLike with Matchers{

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
