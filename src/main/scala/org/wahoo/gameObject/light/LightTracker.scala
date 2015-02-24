package org.wahoo
package gameObject
package light

import
	color.Color,
	math.implicits._,
	math.Scalar,
	shape.Vec,
	collection.OffsetSpaceMatrix

class LightTracker(ambient:Color, nodeSize:Scalar) extends GameObject
{
	// Useful if settings.DrawLightNodes
	override def DrawOrder =
		96

	private val matrix =
		OffsetSpaceMatrix[Color](gameState.cameraSize, nodeSize) { _ =>
			ambient.copy()
		}

	override def step()
	{
		super.step()
		matrix.offset = gameState.camera.leftBottom
		matrix.foreach { _ setTo ambient }
	}

	override def draw()
	{
		super.draw()
		if (Settings.DrawLightNodes)
			render.manyVecs {
				matrix.offset = Vec.Zero
				matrix foreachWithPosition { (color, vec) =>
					render preparedVec (vec, color)
				}
			}
	}

	def alterColorAt(v:Vec)(f:Color => Unit)
	{
		(matrix get v) foreach f
	}

	def alterColors(el:EmitsLight)(f:(Color, Vec) => Unit)
	{
		matrix.foreachInShapeWithPosition(el.lightRelevantArea)(f)
	}

	def colorAt(x:Scalar, y:Scalar): Color =
		if (Settings.LightsOn)
			matrix getLimited (x, y)
		else
			Color.White

	def colorAt(v:Vec): Color =
		colorAt(v.x, v.y)
}
