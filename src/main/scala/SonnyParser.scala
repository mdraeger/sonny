package org.draegisoft.sonny

import scala.util.parsing.combinator._

class SonnyParser extends RegexParsers {
    override def skipWhitespace = false

    def equals       = (space?) ~ "=" ~ (space?)
    def string       = doubleString | singleString
    def charData     = "[^<]+".r
    def space        = """\s+""".r
    def name         = """(:|\w)(\-|\.|\d|:|\w)*""".r 
    def doubleString = "\"" ~> "[^\"]*".r <~ "\""
    def singleString = "'" ~> "[^']*".r <~ "'"
    def separator    = eoi | eol
    def eol          = sys.props("line.separator").r
    def eoi          = """\z""".r
}
