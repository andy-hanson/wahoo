package org.wahoo
package resource

import util.memoizeWeak

import org.newdawn.slick.openal.{Audio, AudioLoader}
import org.newdawn.slick.util.ResourceLoader

object Sound {
	val MainDirectory =
		Resource.MainDirectory + "/sound"

	val TestDirectory =
		Resource.TestDirectory + "/sound"

	def extension =
		"wav"

	def apply(
		subDirectory:String,
		fileName:String,
		loop:Boolean = false,
		test:Boolean = false): Sound =
		load(s"$subDirectory/$fileName", loop, test)

	val load =
		memoizeWeak[(String, Boolean, Boolean), Sound] { nameLoopTest =>
			val (fileName, loop, test) =
				nameLoopTest

			val directory =
				if (test)
					TestDirectory
				else
					MainDirectory

			val path =
				s"$directory/$fileName.$extension"

			val stream =
				ResourceLoader getResourceAsStream path

			Sound(
				fileName,
				loop,
				AudioLoader getAudio (extension.toUpperCase, stream))
	}
}

case class Sound private[resource](
	name:String,
	loop:Boolean,
	data:Audio)
{
	def play(volume:Float =1.0f)
	{
		if (!Settings.SoundEffectsOn)
			return

		val pitch =
			1.0f

		data playAsSoundEffect (pitch, volume, loop)
	}
}
