package org.wahoo
package state
package spacialStore

import shape.Rect

private[spacialStore] case class LooseQuadTreeNode(val inner:Rect, val outer:Rect)
	extends SpacialStoreNode
{
	require(outer.contains(inner))
	def shape = outer
}
