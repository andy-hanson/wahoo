package org.wahoo
package gameObject
package behavior

import shape.Rect

/** An object composed of many child objects.
	* Its shape is the smallest rect encompassing all of its children.
	*/
trait Leader
	extends InArenaAndMutableShape
{
	type Child <: InArena

	val childs =
		scala.collection.mutable.ArrayBuffer.empty[Child]

	def addChild(child:Child)
	{
		childs += child
		export(child)
		shape = Rect containingObjects childs
	}

	override def step()
	{
		super.step()
		shape = Rect containingObjects childs
	}
}
