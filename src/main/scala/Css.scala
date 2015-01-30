package org.draegisoft.sonny

import scala.util.parsing.combinator._

object Css{
  type Stylesheet = List[Rule]
  type Specificity = (Byte, Byte, Byte)

  def toPx(v: Value): Float = v match {
    case Length(l, Px) => l
    case _             => 0
  }
}

class CssParser extends SonnyParser{
   import Css.Stylesheet

   def parseCss: Parser[Stylesheet] = (eol?) ~> 
                                      repsep(rule, (space?) ~ (eol?)) <~
                                      (eol?)

   private def rule: Parser[Rule] = 
      (repsep(selector, ',' ~ (space?)) <~ (space?)) ~ 
       declarations ^^ 
         { case selectors ~ declarations => Rule(selectors, declarations) }

   private def selector: Parser[Selector] = 
      rep(class_ | id | univ | tag) ^^ { 
         case l => l.foldLeft(NilS()) {
           case (simple, "univ") => simple
           case (Simple(n, _, cs), ("id", i: String)) => Simple(n, Some(i), cs)
           case (Simple(_, i, cs), ("tag", t: String)) =>
                   Simple(Some(t), i, cs)
           case (Simple(n, i, cs), ("class", c: String)) => 
                   Simple(n, i, cs ++ List(c))
           case _ => NilS()
         }
      }
            
   private def class_ = "." ~> ident ^^ { c => ("class", c) }
   private def id = '#' ~> ident ^^ { i => ("id", i) }
   private def univ = '*' ^^ { _ => "univ" }
   private def tag = ident ^^ { t => ("tag", t) }
   private def ident = """[*@_]?-?[a-zA-Z_][a-zA-Z0-9-_]*""".r

   private def declarations: Parser[List[Declaration]] = 
     '{' ~> repsep(declaration, (space?)) <~ (space?) <~ '}'

   private def declaration: Parser[Declaration] = 
     ((space?) ~> ident <~ (space?) <~ ':' <~ (space?)) ~
     (value <~ (space?) <~ ';') ^^ { case n ~ v => Declaration(n, v) }

   private def value: Parser[Value] = len | color | keyword

   private def len: Parser[Value] = float ~ unit ^^ { case f~u => Length(f,u) }

   private def float: Parser[Float] = """[\d\.]+""".r ^^ 
                                      { java.lang.Float.parseFloat(_) }

   private def unit: Parser[Unit] = "(p|P)(x|X)".r ^^ { case _ => Px }
   
   private def color: Parser[Value] = '#' ~> """[0-9a-fA-F]{6}""".r ^^
                                       {case s =>
                                        val r = java.lang.Short.parseShort(
                                                 s take (2), 16)
                                        val g = java.lang.Short.parseShort(
                                                 (s drop (2)) take (2), 16)
                                        val b = java.lang.Short.parseShort(
                                                 (s drop (4)) take (2), 16)
                                        ColorValue(Color(r, g, b, 255)) }

   private def keyword: Parser[Value] = ident ^^ { case s => Keyword(s) }
}

case class Rule(val selector: List[Selector], 
                val declaration: List[Declaration])

abstract class Selector {
  import Css.Specificity

  def spec: Specificity
}

object SelectorOrdering extends Ordering[Selector]{
  /** 
    * define an ordering on selectors by lexigographically ordering
    * their specificity.
    */
  def compare (a: Selector, b: Selector) = 
    implicitly[Ordering[Tuple3[Byte,Byte,Byte]]].compare(a.spec, b.spec)
}

case class Simple (val tag: Option[String],
                   val id:  Option[String],
                   val cls: List[String]) extends Selector {

  /** calculate the specificity of this selector */
  override def spec = (id map (_.length.toByte) getOrElse (0),
                       cls.size.toByte,
                       tag map  (_.length.toByte) getOrElse (0))
  
  override def toString = "Tag: %s, Id: %s, Classes: [%s]".
                           format(tag, id, cls mkString (", "))
}

/** an empty selector */
object NilS extends Simple(None, None, Nil){
  def apply() = new Simple(None, None, Nil)
}

case class Declaration(name: String, value: Value)

sealed abstract class Value

case class Keyword(val keyword: String) extends Value {
  override def toString = "Keyword: " + keyword
}

case class Length(val value: Float, val unit: Unit) extends Value {
  override def toString = "Length: %f %s".format (value, unit)
}

case class ColorValue(val value: Color) extends Value{
  override def toString = "Color: " + value
}

abstract class Unit

case object Px extends Unit {
  override def toString = "Px"
}

case class Color(val r: Short, val g: Short, val b: Short, val a: Short){
  require (r >= 0 && r < 256 && 
           g >= 0 && g < 256 && 
           b >= 0 && b < 256 && 
           a >= 0 && a < 256)  

  override def toString = "#%2h%2h%2h%2h".format(r, g, b, a)
}
