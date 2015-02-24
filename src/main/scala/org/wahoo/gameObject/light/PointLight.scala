package org.wahoo
package gameObject
package light

import
	color.Color,
	math.implicits._,
	math.Scalar,
	shape.{Rect, Vec}

/** A point light distributes light through the inverse square of distance. */
trait PointLight extends EmitsLight
{
	def lightPoint  =
		shape.center

	def lightRadius: Scalar
	def light: Color

	def applyLight()
	{
		val lightTimesRadiusSquared =
			light * lightRadius.square

		lightTracker.alterColors(this) { (color, vec) =>
			color += lightTimesRadiusSquared / (lightPoint distance2To vec)
		}
	}

	def lightRelevantArea =
		Rect.centered(lightPoint, lightRadius * 8)
}
