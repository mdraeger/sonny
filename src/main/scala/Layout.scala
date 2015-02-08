package org.draegisoft.sonny

import Style.{PropertyMap, StyledNode, display}

case class Dimensions( val x:       Float
                     , val y:       Float
                     , val width:   Float
                     , val height:  Float
                     , val padding: EdgeSize
                     , val border:  EdgeSize
                     , val margin:  EdgeSize)

case class EdgeSize( val left:   Float
                   , val right:  Float
                   , val top:    Float
                   , val bottom: Float)

object Layout {
  type LayoutBox = NTree[(Dimensions, BoxType)]
  type StyledElement = (NodeType, PropertyMap)

  def buildLayoutTree(root: StyledNode): Either[String, LayoutBox] = {
    def addDim(b: BoxType) = (DefaultDim, b)

    def blt(node: StyledNode): NTree[BoxType] = {
      // utility function, mimicking Haskell's Data.List.groupBy
      def hGroupBy[A](f: A=> (A => Boolean), list: List[A]): List[List[A]] =
        list match {
          case Nil => Nil
          case x :: xs =>
            val t = xs span f(x)
            (x :: t._1) :: hGroupBy(f, t._2)
        }

      def anonify(boxNodes: List[NTree[BoxType]]): List[NTree[BoxType]] = {
        val groupCriterion: NTree[BoxType] => NTree[BoxType] => Boolean = 
          s1 => s2 => isInline(s1) && isInline(s2)
        hGroupBy(groupCriterion, boxNodes) flatMap(mergeInlines)
      }

      def mergeInlines(nodes: List[NTree[BoxType]]): List[NTree[BoxType]] = {
        if (isInline(nodes.head)) List(NTree(AnonymousBlock, nodes)) else nodes
      }

      def isInline(node: NTree[BoxType]): Boolean = node match {
        case NTree(InlineNode(_), _) => true
        case _                       => false
      }

      val (nd, cs) = node match {
        case NTree(nd, cs) => (nd, cs)
      }

      val ns_ = cs.filter(c => display(c) != DisplayNone).map(blt)

      val (n, ns) = display(node) match {
        case Block  => (BlockNode(nd), anonify(ns_))
        case Inline => (InlineNode(nd), ns_)
        case _      => ??? // can never happen
      }

      NTree(n, ns)
    }

    display(root) match {
      case Block       => Right(blt(root).map(addDim))
      case Inline      => Right(blt(root).map(addDim))
      case DisplayNone => Left("error: root node has display:none")
    }
  }

  def layout(box: LayoutBox, dim: Dimensions): Either[String, LayoutBox] = 
    box match {
      case NTree((_, boxType), _) => 
        boxType match {
          case BlockNode(_) => layoutBlock(dim, box)
          case InlineNode(_) => ???
          case AnonymousBlock => ???
        }
    }

  def layoutBlock(dim: Dimensions, root: LayoutBox): Either[String, LayoutBox] = {
    def errorString: String => Either[String, LayoutBox] = s => Left(s)
    
    calcWidth(dim, root).fold(
      errorString, box => calcPosition(dim, box)).fold(
        errorString, layoutChildren _).fold(
          errorString, calcHeight _)
  }

  def calcWidth(dim: Dimensions, box: LayoutBox): Either[String, LayoutBox] = ???

  def calcPosition(dim: Dimensions, box: LayoutBox): Either[String, LayoutBox] = ???

  def layoutChildren(box: LayoutBox): Either[String, LayoutBox] = ???

  def calcHeight(box: LayoutBox): Either[String, LayoutBox] = ???

  def getStyledElem(node: LayoutBox): Either[String, StyledNode] = {
    val box = node match { case NTree((_, box), _) => box }
    box match {
      case BlockNode(s) => Right(NTree(s, Nil))
      case InlineNode(s) => Right(NTree(s, Nil))
      case AnonymousBlock => Left("Error: attempted to access the nonexistant StyleNode of an AnonymousBlock")
    }
  }
}

sealed abstract class BoxType

case class BlockNode( val styledElement: Layout.StyledElement) extends BoxType
case class InlineNode( val styledElement: Layout.StyledElement) extends BoxType
case object AnonymousBlock extends BoxType

object EmptyEdge extends EdgeSize(0,0,0,0)

object DefaultDim extends Dimensions(0,0,0,0, EmptyEdge, EmptyEdge, EmptyEdge)
