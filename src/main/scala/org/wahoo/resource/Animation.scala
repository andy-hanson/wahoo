package org.wahoo
package resource

import shape.Vec

object Animation {
	def apply(data:AnimationData): Animation =
		data match {
			case m:MovingAnimationData =>
				if (m.loop)
					new MovingAnimationLoop(m)
				else
					new MovingAnimationCanEnd(m)
			case s:StillAnimationData  =>
				if (s.loop)
					new StillAnimationLoop(s)
				else
					new StillAnimationCanEnd(s)
			case NoAnimationData =>
				NoAnimation
		}
}

abstract class Animation
{
	def data: AnimationData
	def step()
	{
		advanceTime()
	}
	def advanceTime()
	def frame: Image
	def canEnd: Boolean
	def over: Boolean
	def size: Vec
	def width =
		frame.width
	def height =
		frame.height
	def changesSize: Boolean
	def synchronized = false
}

abstract class AbstractMovingAnimation(val data:MovingAnimationData)
	extends Animation
{
	var timeUntilNewFrame =
		data.frameTime
	var frameIndex =
		0
	def changesSize =
		data.changesSize

	def advanceTime()
	{
		timeUntilNewFrame -= 1

		if (timeUntilNewFrame == 0) {
			timeUntilNewFrame = data.frameTime
			advanceFrame()
		}
	}

	def advanceFrame()
	{
		frameIndex += 1
		if (frameIndex == data.nFrames) onFrameIndexPastEnd()
	}

	def onFrameIndexPastEnd()

	def frame =
		data.frames(frameIndex)

	def size = data.sizes(frameIndex)
}

class MovingAnimationCanEnd(data:MovingAnimationData) extends
	AbstractMovingAnimation(data)
{
	def canEnd = true
	var over = false

	override def advanceTime()
	{
		if (!over)
			super.advanceTime()
	}

	def onFrameIndexPastEnd()
	{
		over = true
		frameIndex -= 1
	}
}

class MovingAnimationLoop(data:MovingAnimationData)
	extends AbstractMovingAnimation(data)
{
	def canEnd = false
	def over   = false

	def onFrameIndexPastEnd()
	{
		frameIndex = 0
	}
}

abstract class StillAnimation extends Animation
{
	def data: StillAnimationData
	def frame =
		data.image
	def changesSize =
		false
	def size =
		data.size
}

class StillAnimationLoop(val data:StillAnimationData) extends StillAnimation {
	def advanceTime() { }
	def canEnd =
		false
	def over =
		false
}

class StillAnimationCanEnd(val data:StillAnimationData) extends StillAnimation {
	var time = 0
	def advanceTime()
	{
		time += 1
	}
	def canEnd =
		true
	def over =
		time > data.frameTime
}

object NoAnimation extends Animation {
	def advanceTime() { }
	def data =
		null
	def changesSize =
		false
	def frame =
		NoImage
	def canEnd =
		false
	def over =
		false
	def size =
		Vec.Zero
}
