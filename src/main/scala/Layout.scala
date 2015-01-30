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
      val (nd, cs) = node match {
        case NTree(nd, cs) => (nd, cs)
      }

      val ns_ = cs.filter(c => display(c) != DisplayNone).map(blt)

      def anonify(boxNode: List[NTree[BoxType]]): List[NTree[BoxType]] = ???

      val (n, ns) = display(node) match {
        case Block  => (BlockNode(nd), anonify(ns_))
        case Inline => (InlineNode(nd), ns_)
        case _      => ??? // can never happen
      }

      ???
    }

    display(root) match {
      case Block       => Right(blt(root).map(addDim))
      case Inline      => Right(blt(root).map(addDim))
      case DisplayNone => Left("error: root node has display:none")
    }
  }
}

sealed abstract class BoxType

case class BlockNode( val styledElement: Layout.StyledElement) extends BoxType
case class InlineNode( val styledElement: Layout.StyledElement) extends BoxType
case object AnonymousBlock extends BoxType

object EmptyEdge extends EdgeSize(0,0,0,0)

object DefaultDim extends Dimensions(0,0,0,0, EmptyEdge, EmptyEdge, EmptyEdge)
