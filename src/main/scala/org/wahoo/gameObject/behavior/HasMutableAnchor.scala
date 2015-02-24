package org.wahoo
package gameObject
package behavior

import shape.Anchor

trait HasMutableAnchor extends Sprite
{
	def StartAnchor: Anchor =
		Anchor.Center

	private var _anchor: Anchor = StartAnchor

	override def anchor =
		_anchor

	def anchor_=(newAnchor:Anchor)
	{
		_anchor = newAnchor
	}
}
