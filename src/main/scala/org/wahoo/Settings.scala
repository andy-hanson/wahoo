package org.wahoo

object Settings
{
	/** If true, objects are drawn whether their SpacialStore node is onscreen or not. */
	val SpacialStoreLessDraw = false

	/** If true, objects update whether their SpacialStore node is near the screen or not. */
	val SpacialStoreLessStep = false

	/** If true, objects will check all other objects for collisions, not just nearby ones. */
	val SpacialStoreLessCollide = false

	/** If true, the shape of each object is drawn along with the object itself. */
	val DrawShapes = false

	/** If true, the SpacialStore will draw where its borders are. */
	val DrawSpacialStore = false

	/** Whether sound effects play. */
	val SoundEffectsOn = true

	/** Whether objects are drawn shaded by light. */
	val LightsOn = true

	/** If true, the light at each of lightTracker's nodes is drawn explicitly as a point. */
	val DrawLightNodes = false

	/** Whether the camera goes immediately to the object it's following. */
	val InstantCamera = true
}
