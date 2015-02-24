package org.wahoo
package gameObject

import shape.Shape

trait InArenaAndMutableShape
	extends InArena
	with HasMutableShape
{
	override def shape_=(newShape:Shape)
	{
		super.shape_=(newShape)
		spacialStore.mayNeedToMove(this)
	}
}
