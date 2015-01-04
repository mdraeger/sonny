package org.draegisoft.sonny

import Style.PropertyMap

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
}

sealed abstract class BoxType

case class BlockNode( val styledElement: Layout.StyledElement) extends BoxType
case class InlineNode( val styledElement: Layout.StyledElement) extends BoxType
case class AnonymousBlock() extends BoxType

object EmptyEdge extends EdgeSize(0,0,0,0)

object DefaultDim extends Dimensions(0,0,0,0, EmptyEdge, EmptyEdge, EmptyEdge)
