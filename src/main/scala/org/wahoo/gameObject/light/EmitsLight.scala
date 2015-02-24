package org.wahoo
package gameObject
package light

import
	color.Color,
	shape.Shape

trait EmitsLight
	extends InArena
{
	/** Area where applyLight will be used. */
	def lightRelevantArea: Shape

	/** Apply myself to the lightTracker. */
	def applyLight()

	abstract override def step()
	{
		super.step()
		applyLight()
	}
}
