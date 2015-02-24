package org.wahoo
package gameObject
package graphic

import
	math.Scalar,
	math.implicits._,
	shape.Vec

class Background
	extends Sprite(Vec.Zero)
{
	override def DrawOrder =
		-127

	def AniIdle =
		ani("idle")

	shape =
		shape withCenter camera.size.half
}

class NamedBackground(name:String)
	extends Background
{
	override def AnimationFullDirectory =
		name
}

trait PartlyScrollingX
	extends HasMutableShape
{
	def minL: Scalar =
		0

	def maxL =
		camera.width - width

	private def camMinL =
		camera.region.left

	private def camMaxL =
		camera.region.right - camera.width

	private def camScrollFactorX  =
		((camera.left - camMinL) / (camMaxL - camMinL))

	override def step()
	{
		super.step()
		left = minL + (maxL - minL) * camScrollFactorX
	}
}

trait PartlyScrollingY
	extends HasMutableShape
{
	def minB: Scalar =
		0
	def maxB =
		camera.height - height

	private def camMinB =
		camera.region.bottom
	private def camMaxB =
		camera.region.top - camera.height
	private def camScrollFactorY =
		((camera.bottom - camMinB) / (camMaxB - camMinB))

	override def step()
	{
		super.step()
		bottom = minB + (maxB - minB) * camScrollFactorY
	}
}

class PartlyScrollingBillboard(scale:Scalar = 1)
	extends Sprite(Vec.Zero)
	with PartlyScrollingX
	with PartlyScrollingY
{
	def AniIdle = ani("idle", scale = scale)
}

class PartlyScrollingBackground(scale:Scalar = 1)
	extends PartlyScrollingBillboard(scale)
{
	override def DrawOrder = -127
}

class PartlyScrollingForeground(scale:Scalar = 1)
	extends PartlyScrollingBillboard(scale)
{
	override def DrawOrder = 64
}
