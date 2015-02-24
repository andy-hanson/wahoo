package org.wahoo
package resource

import
	math.Scalar,
	math.implicits._,
	shape.Vec,
	 util.memoizeWeak

/** @todo just use scala io */
import
	scala.io.Source,
	java.io.File

/** Class for holding any description of a failure to load an animation.
  * Ex. file missing, file corrupt
  */
case class AnimationLoadException(description:String) extends Exception { }

/** Class to represent the settings for an animation.
  * nFrames: How many frames it contains.
  * frameTime: (MovingAnimation only)
  *   How many ticks of the game clock between animation frames.
  * loop: (MovingAnimation only)
  *   Whether this animation restarts upon ending.
  * @todo light
  */
case class AnimationSettings(
	frames: Int,
	frameTime: Int,
	scale: Scalar,
	loop: Boolean,
	shapeShrink : Vec,
	synchronized: Boolean)

/** Factory for getting Animations.
	*/
object AnimationData
{
	val MainDirectory =
		s"${Resource.MainDirectory}/animation"
	val TestDirectory =
		s"${Resource.TestDirectory}/animation"

	def apply(
		subDirectory: String,
		name: String,
		frames: Int     = 1,
		frameTime: Int     = 30,
		scale: Scalar  = 1.0,
		loop: Boolean = true,
		shapeShrink: (Scalar, Scalar) = (0.0, 0.0),
		synchronized: Boolean = true,
		test: Boolean = false) =
	{
		val directory =
			if (test)
				TestDirectory
			else
				MainDirectory

		val path =
			s"$directory/$subDirectory"

		val shrink =
			Vec fromScalarPair shapeShrink

		val settings =
			AnimationSettings(frames, frameTime, scale, loop, shrink, synchronized)

		load((path, name, settings))
	}

	/** Loads the animation of a given directory name, name, and settings.
		* Loading uses weak memoization, so unused animations will be deleted.
		*/
	val load =
		memoizeWeak { dirNameAndSettings: (String, String, AnimationSettings) =>
			val (directory, name, settings) = dirNameAndSettings

			def idleExists() =
				new File(s"$directory.png").exists || new File(s"$directory/0.png").exists

			val aniDir =
				if (name == "idle" && idleExists())
					directory
				else
					s"$directory/$name"

			if (settings.frames == 1)
				loadImageAnimation(aniDir, settings)
			else
				loadMovingAnimation(aniDir, settings)
	}

	/** Load a single-image animation. */
	private def loadImageAnimation(directory:String, settings:AnimationSettings) =
	{
		require(!settings.synchronized)
		StillAnimationData(
			directory,
			settings.frameTime,
			settings.loop,
			settings.scale,
			settings.shapeShrink)
	}

	/** Load multiple images for a moving animation. */
	private def loadMovingAnimation(directory:String, settings:AnimationSettings) =
	{
		//Load all of the images from 0 to frames - 1.
		if (settings.frames == 0)
			throw AnimationLoadException(s"Animation $directory needs nFrames!")

		MovingAnimationData(
			directory,
			settings.frames,
			settings.frameTime,
			settings.loop,
			settings.scale,
			settings.shapeShrink,
			settings.synchronized)
	}
}


sealed abstract class AnimationData {
	//def changesSize          : Boolean
	//def size       (time:Int) = frame(time).size
	def loop: Boolean
	val canEnd = !loop
	def synchronized: Boolean
}


case class MovingAnimationData(name        : String,
															 nFrames     : Int,
															 frameTime   : Int,
															 loop        : Boolean,
															 scale       : Scalar,
															 shapeShrink : Vec,
															 synchronized: Boolean) extends AnimationData {

	val frames = (0 until nFrames).map
		{ n => Image(s"$name/$n", scale) }.toArray

	val sizes = frames.map(_.size - shapeShrink)
	assert(sizes.forall(_.validSize), s"$this can not shrink!")

	val changesSize = !frames.forall { _.size == frames(0).size }

	def frame(index:Int) = frames(index)
}

case class StillAnimationData(name       : String,
															frameTime  : Int,
															loop       : Boolean,
															scale      : Scalar,
															shapeShrink: Vec) extends AnimationData {
	val image = Image(name, scale)
	val size = image.size - shapeShrink
	assert(size.validSize, s"$this can not shrink!")
	def synchronized = false
}

object NoAnimationData extends AnimationData {
	def loop = true
	def synchronized = false
}
